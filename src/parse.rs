use std::uint;
use ty;
use ty::Context;
use ty::Describe;

//////////////////////////////////////////////////////////////////////////////
// Simple parser combinator interface

pub trait Parse<T> {
    fn parse(&self,
             cx: &mut ty::Context,
             input: &[u8],
             start: uint)
             -> ParseError<(uint, T)>;
}

pub type ParseError<T> = Result<T, uint>;

pub type Parser<T> = ~Parse:'static<T>;

fn obj<T,R:'static+Parse<T>>(r: R) -> Parser<T> {
    ~r as Parser<T>
}

//////////////////////////////////////////////////////////////////////////////

fn is_digit(c: char) -> bool {
    match c {
        '0' .. '9' => true,
        _ => false
    }
}

fn is_ident_start(c: char) -> bool {
    match c {
        'a' .. 'z' | 'A' .. 'Z' | '_' => true,
        _ => false
    }
}

fn is_ident_cont(c: char) -> bool {
    match c {
        '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' => true,
        _ => false
    }
}

fn is_oper(c: char) -> bool {
    match c {
        '+' | '-' | '>' | '<' | '*' | '/' => true,
        _ => false
    }
}

fn accumulate(buf: &mut ~str,
              input: &[u8],
              start: uint,
              test: &fn(char) -> bool)
              -> uint {
    let mut i = start;
    while i < input.len() {
        let c = input[i] as char;
        if test(c) {
            buf.push_char(c);
            i += 1;
        } else {
            break;
        }
    }
    return i;
}

fn skip_whitespace(input: &[u8], start: uint) -> uint {
    let mut i = start;
    while i < input.len() {
        match input[i] as char {
            ' ' => { i += 1; }
            '\t' => { i += 1; }
            '\n' => { i += 1; }
            '\r' => { i += 1; }
            _ => { break; }
        }
    }
    i
}

//////////////////////////////////////////////////////////////////////////////

struct Nothing1;

fn Nothing() -> Parser<()> {
    obj(Nothing1)
}

impl Parse<()> for Nothing1 {
    fn parse(&self,
             _: &mut ty::Context,
             _: &[u8],
             start: uint)
             -> ParseError<(uint, ())> {
        Ok((start, ()))
    }
}

//////////////////////////////////////////////////////////////////////////////

struct Integer1;

fn Integer() -> Parser<uint> {
    obj(Integer1)
}

impl Parse<uint> for Integer1 {
    fn parse(&self,
             _: &mut ty::Context,
             input: &[u8],
             start: uint)
             -> Result<(uint, uint), uint> {
        let start = skip_whitespace(input, start);
        let mut i = start;
        let mut r: uint = 0;
        while i < input.len() {
            if is_digit(input[i] as char) {
                r = r * 10 + (input[i] as uint) - ('0' as uint);
                i += 1;
            } else {
                break;
            }
        }

        if i == start {
            Err(start)
        } else {
            Ok((i, r))
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

struct Ident1;

fn Ident() -> Parser<ty::Id> {
    obj(Ident1)
}

impl Parse<ty::Id> for Ident1 {
    fn parse(&self,
             cx: &mut ty::Context,
             input: &[u8],
             start: uint)
             -> ParseError<(uint, ty::Id)> {
        let mut buf = ~"";
        let start = skip_whitespace(input, start);

        if start == input.len() {
            return Err(start);
        }

        let c0 = input[start] as char;
        let end = if is_ident_start(c0) {
            buf.push_char(c0);
            accumulate(&mut buf, input, start+1, is_ident_cont)
        } else if c0 == '(' {
            buf.push_char('(');
            let end = accumulate(&mut buf, input, start+1, is_oper);
            if end < input.len() && input[end] as char == ')' {
                buf.push_char(')');
                end + 1
            } else {
                start
            }
        } else {
            start
        };

        if start == end {
            Err(start)
        } else {
            let id = cx.interner.id(buf);
            Ok((end, id))
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// Repeat: Parse over and over.

pub struct Repeat<T> {
    sub: Parser<T>,
    min: uint
}

pub fn Repeat<T>(sub: Parser<T>, min: uint) -> Parser<~[T]> {
    obj(Repeat { sub: sub, min: min })
}

impl<T> Parse<~[T]> for Repeat<T> {
    fn parse(&self,
             cx: &mut ty::Context,
             input: &[u8],
             start: uint)
             -> ParseError<(uint, ~[T])> {
        let mut pos = start;
        let mut result = ~[];
        loop {
            match self.sub.parse(cx, input, pos) {
                Ok((end, v)) => {
                    result.push(v);
                    pos = end;
                }
                Err(e) => {
                    if result.len() >= self.min {
                        return Ok((pos, result));
                    } else {
                        return Err(e);
                    }
                }
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// Map: Parse then apply a (bare) fn.

type Fn<T,U> = extern "Rust" fn(T) -> U;

pub struct Map<T,U> {
    sub: Parser<T>,
    f: Fn<T,U>
}

pub fn Map<T,U>(sub: Parser<T>, f: Fn<T,U>) -> Parser<U> {
    obj(Map { sub: sub, f: f })
}

impl<T,U> Parse<U> for Map<T,U> {
    fn parse(&self,
             cx: &mut ty::Context,
             input: &[u8],
             start: uint)
             -> ParseError<(uint, U)> {
        match self.sub.parse(cx, input, start) {
            Ok((end, v)) => {
                Ok((end, (self.f)(v)))
            }
            Err(e) => {
                Err(e)
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// Choice: Tries many parsers in turn.

pub struct Choice<T> {
    choices: ~[Parser<T>]
}

pub fn Choice<T>(choices: ~[Parser<T>]) -> Parser<T> {
    obj(Choice { choices: choices })
}

pub fn Choice2<T>(left: Parser<T>, right: Parser<T>) -> Parser<T> {
    Choice(~[left, right])
}

impl<T> Parse<T> for Choice<T> {
    fn parse(&self,
             cx: &mut ty::Context,
             input: &[u8],
             start: uint)
             -> ParseError<(uint, T)> {
        let mut farthest = start;
        for choice in self.choices.iter() {
            match choice.parse(cx, input, start) {
                Ok((end, v)) => {
                    return Ok((end, v));
                }
                Err(e) => {
                    farthest = uint::max(e, farthest);
                }
            }
        }
        return Err(farthest);
    }
}

//////////////////////////////////////////////////////////////////////////////
// Tuple: Parses with `first` then `second`, returning tuple of results.

pub struct Tuple<T,U> {
    first: Parser<T>,
    second: Parser<U>
}

pub fn Tuple<T,U>(first: Parser<T>, second: Parser<U>) -> Parser<(T,U)> {
    obj(Tuple { first: first, second: second })
}

impl<T,U> Parse<(T,U)> for Tuple<T,U> {
    fn parse(&self,
             cx: &mut ty::Context,
             input: &[u8],
             start: uint)
             -> ParseError<(uint, (T,U))> {
        match self.first.parse(cx, input, start) {
            Ok((_, a)) => {
                match self.second.parse(cx, input, start) {
                    Ok((end, b)) => {
                        Ok((end, (a,b)))
                    }
                    Err(e) => {
                        Err(e)
                    }
                }
            }
            Err(e) => {
                Err(e)
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// Predicate: Parses with `first` then `second`, ignoring result of `first`.

pub struct Predicate<T,U> {
    first: Parser<T>,
    second: Parser<U>
}

pub fn Predicate<T,U>(first: Parser<T>, second: Parser<U>) -> Parser<U> {
    obj(Predicate { first: first, second: second })
}

impl<T,U> Parse<U> for Predicate<T,U> {
    fn parse(&self,
             cx: &mut ty::Context,
             input: &[u8],
             start: uint)
             -> ParseError<(uint, U)> {
        match self.first.parse(cx, input, start) {
            Ok((_, _)) => {
                match self.second.parse(cx, input, start) {
                    Ok((end, b)) => {
                        Ok((end, b))
                    }
                    Err(e) => {
                        Err(e)
                    }
                }
            }
            Err(e) => {
                Err(e)
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// PostPredicate: Parses with `first` then tests whether `second` can parse,
// but result of `second` is dropped (and `second` does not consume text).

pub struct PostPredicate<T,U> {
    first: Parser<T>,
    second: Parser<U>
}

pub fn PostPredicate<T,U>(first: Parser<T>,
                          second: Parser<U>)
                          -> Parser<T> {
    obj(PostPredicate { first: first, second: second })
}

impl<T,U> Parse<T> for PostPredicate<T,U> {
    fn parse(&self,
             cx: &mut ty::Context,
             input: &[u8],
             start: uint)
             -> ParseError<(uint, T)> {
        match self.first.parse(cx, input, start) {
            Ok((pos, a)) => {
                match self.second.parse(cx, input, start) {
                    Ok((_, _)) => {
                        Ok((pos, a))
                    }
                    Err(e) => {
                        Err(e)
                    }
                }
            }
            Err(e) => {
                Err(e)
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// Not: Succeeds if `test` fails. Consumes nothing.

pub struct Not<T> {
    test: Parser<T>,
}

pub fn Not<T>(test: Parser<T>) -> Parser<()> {
    obj(Not { test: test })
}

impl<T> Parse<()> for Not<T> {
    fn parse(&self,
             cx: &mut ty::Context,
             input: &[u8],
             start: uint)
             -> ParseError<(uint, ())> {
        match self.test.parse(cx, input, start) {
            Ok((pos, _)) => {
                Err(pos)
            }
            Err(_) => {
                Ok((start, ()))
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// Convenient methods

pub trait Convenience<T> {
    fn rep(self, min: uint) -> Parser<~[T]>;
    fn star(self) -> Parser<~[T]>;
    fn plus(self) -> Parser<~[T]>;
    fn seq<U>(self, u: Parser<U>) -> Parser<(T,U)>;
    fn then<U>(self, u: Parser<U>) -> Parser<U>;
    fn map<U>(self, f: Fn<T,U>) -> Parser<U>;
    fn test<U>(self, p: Parser<U>) -> Parser<T>;
}

impl<T> Convenience<T> for Parser<T> {
    fn rep(self, min: uint) -> Parser<~[T]> {
        Repeat(self, min)
    }

    fn star(self) -> Parser<~[T]> {
        Repeat(self, 0)
    }

    fn plus(self) -> Parser<~[T]> {
        Repeat(self, 1)
    }

    fn seq<U>(self, u: Parser<U>) -> Parser<(T,U)> {
        Tuple(self, u)
    }

    fn then<U>(self, u: Parser<U>) -> Parser<U> {
        Predicate(self, u)
    }

    fn map<U>(self, f: Fn<T,U>) -> Parser<U> {
        Map(self, f)
    }

    fn test<U>(self, p: Parser<U>) -> Parser<T> {
        PostPredicate(self, p)
    }
}

//////////////////////////////////////////////////////////////////////////////

pub fn parse<T>(cx: &mut ty::Context,
                text: &[u8],
                parser: &Parser<T>)
                -> ParseError<T> {
    match parser.parse(cx, text, 0) {
        Err(e) => Err(e),
        Ok((end, v)) => {
            let end1 = skip_whitespace(text, end);
            if end1 != text.len() {
                Err(end)
            } else {
                Ok(v)
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// Tests

#[cfg(test)]
fn test<T:Describe>(text: &'static str,
                    parser: &Parser<T>,
                    expected: &'static str) {
    let bytes = text.as_bytes();
    let mut cx = Context::new();
    match parse(&mut cx, bytes, parser) {
        Err(idx) => {
            fail!(format!("Parse error at index {}", idx))
        }
        Ok(v) => {
            let description = v.mk_str(&mut cx);
            assert_eq!(description.slice_from(0), expected);
        }
    }
}

#[cfg(test)]
fn test_err<T:Describe>(text: &'static str,
                        parser: &Parser<T>,
                        expected: uint) {
    let bytes = text.as_bytes();
    let mut cx = Context::new();
    match parse(&mut cx, bytes, parser) {
        Err(index) => {
            assert_eq!(index, expected);
        }
        Ok(v) => {
            let description = v.mk_str(&mut cx);
            fail!(format!("Parse succeeded with {}", description));
        }
    }
}

#[test]
fn idents() {
    let parser = Ident().rep(1);
    test(" hello    world", &parser, "[hello,world]");
    test_err("", &parser, 0);
    test_err("h 1", &parser, 1);
}

#[test]
fn digits() {
    let parser = Integer().rep(1);
    test(" 12 24   36", &parser, "[12,24,36]");
    test_err("", &parser, 0);
    test_err("1 h", &parser, 1);
}

#[test]
fn idents_or_digits() {
    pub enum Choice { IsIdent(ty::Id), IsNumber(uint) }

    impl ty::Describe for Choice {
        fn describe(&self, cx: &Context, out: &mut ~str) {
            match *self {
                IsIdent(i) => i.describe(cx, out),
                IsNumber(i) => i.describe(cx, out),
            }
        }
    }

    let parser =
        Choice(~[
            Integer().map(IsNumber),
            Ident().map(IsIdent)]).rep(1);
    test(" 12 24 hi  36", &parser, "[12,24,hi,36]");
    test_err("--", &parser, 0);
}
