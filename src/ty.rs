use err;
use err::Fallible;
use std::hashmap::HashMap;

pub struct Context {
    interner: Interner,
    k_star: @Kind,
    types: StandardTypes,
}

pub struct Interner {
    identifiers: ~[~str],
}

pub struct StandardTypes {
    t_unit: @Type,
    t_char: @Type,
    t_int: @Type,
    t_integer: @Type,
    t_float: @Type,
    t_double: @Type,
    t_list: @Type,
    t_arrow: @Type,
    t_tuple2: @Type,
}

#[deriving(Eq,Clone)]
pub struct Id {
    repr: uint // index into context's identifiers list
}

fn Id(u: uint) -> Id {
    Id { repr: u }
}

#[deriving(Eq)]
pub enum Kind {
    Star,
    KFun(@Kind, @Kind),
}

#[deriving(Eq)]
pub enum Type {
    TVar(Tyvar),
    TCon(Tycon),
    TAp(@Type, @Type),
    TGen(uint),
}

#[deriving(Eq,Clone)]
pub struct Tyvar {
    id: Id,
    kind: @Kind
}

#[deriving(Eq)]
pub struct Tycon {
    id: Id,
    kind: @Kind
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            identifiers: ~[]
        }
    }

    pub fn id(&mut self, s: &str) -> Id {
        match self.identifiers.iter().enumerate().find(|&(_,p)| p.slice(0, p.len()) == s) {
            Some((i,_)) => Id(i),
            None => {
                self.identifiers.push(s.to_owned());
                Id(self.identifiers.len() - 1)
            }
        }
    }

    pub fn int_identifier(&mut self, i: uint) -> Id {
        self.id(format!("{}", i))
    }
}

impl Context {
    fn new() -> Context {
        let mut interner = Interner::new();

        let k_star = @Star;

        let types = StandardTypes {
            t_unit: @TCon(Tycon { id: interner.id("()"), kind: k_star }),
            t_char: @TCon(Tycon { id: interner.id("Char"), kind: k_star }),
            t_int: @TCon(Tycon { id: interner.id("Int"), kind: k_star }),
            t_integer: @TCon(Tycon { id: interner.id("Integer"), kind: k_star }),
            t_float: @TCon(Tycon { id: interner.id("Float"), kind: k_star }),
            t_double: @TCon(Tycon { id: interner.id("Double"), kind: k_star }),

            t_list: @TCon(Tycon { id: interner.id("[]"), kind: @KFun(k_star, k_star) }),
            t_arrow: @TCon(Tycon { id: interner.id("(->)"), kind: @KFun(k_star, @KFun(k_star, k_star)) }),
            t_tuple2: @TCon(Tycon { id: interner.id("(,)"), kind: @KFun(k_star, @KFun(k_star, k_star)) }),
        };

        Context {
            interner: interner,
            k_star: k_star,
            types: types
        }
    }

    fn func(&self, input: @Type, output: @Type) -> @Type {
        @TAp(@TAp(self.types.t_arrow, input), output)
    }

    fn list(&self, elem: @Type) -> @Type {
        @TAp(self.types.t_list, elem)
    }

    fn pair(&self, a: @Type, b: @Type) -> @Type {
        @TAp(@TAp(self.types.t_tuple2, a), b)
    }
}

///////////////////////////////////////////////////////////////////////////
// Strings

trait Describe {
    fn mk_str(&self, cx: &Context) -> ~str {
        let mut s = ~"";
        self.describe(cx, &mut s);
        s
    }

    fn describe(&self, cx: &Context, out: &mut ~str);
}

impl Describe for Type {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        match *self {
            TVar(ref tv) => tv.describe(cx, out),
            TCon(ref tc) => tc.describe(cx, out),
            TAp(ref t1, ref t2) => {
                out.push_str("(");
                t1.describe(cx, out);
                out.push_str(" -> ");
                t2.describe(cx, out);
                out.push_str(")");
            }
            TGen(id) => {
                out.push_str(format!("${}", id));
            }
        }
    }
}

impl Describe for Tycon {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        self.id.describe(cx, out);
    }
}

impl Describe for Tyvar {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        out.push_str("'");
        self.id.describe(cx, out);
    }
}

impl Describe for Id {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        out.push_str(cx.interner.identifiers[self.repr]);
    }
}

///////////////////////////////////////////////////////////////////////////
// Computing kinds
//
// Section 4 / Page 7

pub trait HasKind {
    fn kind(&self) -> @Kind;
}

impl HasKind for Type {
    fn kind(&self) -> @Kind {
        match *self {
            TCon(tc) => tc.kind(),
            TVar(v) => v.kind(),
            TAp(t, _) => match *t.kind() {
                KFun(_, out) => out,
                Star => fail!(format!("Invalid kind"))
            },
            TGen(_) => {
                fail!(format!("Unsubstituted generic"))
            }
        }
    }
}

impl HasKind for Tycon {
    fn kind(&self) -> @Kind {
        self.kind
    }
}

impl HasKind for Tyvar {
    fn kind(&self) -> @Kind {
        self.kind
    }
}

///////////////////////////////////////////////////////////////////////////
// Substitutions
//
// Section 5

#[deriving(Clone)]
pub struct SubstPair {
    from: Tyvar,
    to: @Type
}

#[deriving(Clone)]
pub struct Subst {
    pairs: ~[SubstPair]
}

impl Subst {
    pub fn null() -> Subst {
        Subst { pairs: ~[] }
    }

    pub fn from(from: Tyvar, to: @Type) -> Subst {
        let mut s = Subst::null();
        s.add(from, to);
        s
    }

    pub fn add(&mut self, from: Tyvar, to: @Type) {
        assert_eq!(from.kind(), to.kind());
        self.pairs.push(SubstPair { from: from, to: to });
    }

    pub fn lookup(&self, tyvar: Tyvar) -> Option<@Type> {
        for pair in self.pairs.iter() {
            if pair.from.id == tyvar.id {
                return Some(pair.to);
            }
        }
        None
    }
}

pub trait Types {
    fn apply(&self, subst: &Subst) -> Self;
    fn tv(&self) -> ~[Tyvar];
}

impl Types for @Type {
    fn apply(&self, subst: &Subst) -> @Type {
        match **self {
            TVar(tv) => {
                match subst.lookup(tv) {
                    Some(t) => t,
                    None => *self,
                }
            }
            TCon(_) => *self,
            TAp(s, t) => @TAp(s.apply(subst), t.apply(subst)),
            TGen(_) => *self,
        }
    }

    fn tv(&self) -> ~[Tyvar] {
        match **self {
            TVar(tv) => ~[tv],
            TAp(s, t) => union(s.tv(), t.tv()),
            TCon(_) | TGen(_) => ~[],
        }
    }
}

impl<A:Types> Types for ~[A] {
    fn apply(&self, s: &Subst) -> ~[A] {
        self.map(|a| a.apply(s))
    }
    fn tv(&self) -> ~[Tyvar] {
        self.iter().fold(~[], |v, a| union(v, a.tv()))
    }
}

fn union<T:Eq>(v1: ~[T], v2: ~[T]) -> ~[T] {
    let mut v1 = v1;
    for t in v2.move_iter() {
        if !v1.contains(&t) {
            v1.push(t);
        }
    }
    v1
}

pub fn compose(s1: Subst, s2: Subst) -> Subst {
    // Yields a subst equivalent `s` where `v.apply(s)` is
    // equivalent to `v.apply(s2).apply(s1)`
    let mut out = Subst::null();
    for pair in s2.pairs.iter() {
        out.add(pair.from, pair.to.apply(&s1));
    }
    for pair in s1.pairs.iter() {
        if out.lookup(pair.from).is_none() {
            out.add(pair.from, pair.to);
        }
    }
    out
}

pub fn merge(s1: &Subst, s2: &Subst) -> Fallible<Subst> {
    let mut out = (*s1).clone();
    for pair in s2.pairs.iter() {
        if out.lookup(pair.from).is_none() {
            out.add(pair.from, pair.to);
        } else {
            return Err(err::DuplicateBindings(pair.from.id));
        }
    }
    Ok(out)
}
