use cx;
use cx::Context;
use err;
use err::Fallible;
use intern::Id;
use util;

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
}

#[deriving(Eq)]
pub struct Tycon {
    id: Id,
}

pub struct KindDef {
    id: Id,
    kind: @Kind
}

///////////////////////////////////////////////////////////////////////////
// Strings

impl cx::Describe for Type {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        match *self {
            TVar(ref tv) => tv.describe(cx, out),
            TCon(ref tc) => tc.describe(cx, out),
            TAp(ref t1, ref t2) => {
                out.push_str("(");
                t1.describe(cx, out);
                out.push_str(" ");
                t2.describe(cx, out);
                out.push_str(")");
            }
            TGen(id) => {
                out.push_str(format!("${}", id));
            }
        }
    }
}

impl cx::Describe for Kind {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        match *self {
            Star => {
                out.push_str("*");
            }
            KFun(k1, k2) => {
                out.push_str("(");
                k1.describe(cx, out);
                out.push_str(" -> ");
                k2.describe(cx, out);
                out.push_str(")");
            }
        }
    }
}

impl cx::Describe for KindDef {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        self.id.describe(cx, out);
        out.push_str(" :: ");
        self.kind.describe(cx, out);
    }
}

impl cx::Describe for Tycon {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        self.id.describe(cx, out);
    }
}

impl cx::Describe for Tyvar {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        out.push_str("'");
        self.id.describe(cx, out);
    }
}

///////////////////////////////////////////////////////////////////////////
// Computing kinds
//
// Section 4 / Page 7

pub trait HasKind {
    fn kind(&self, cx: &Context) -> @Kind;
}

impl HasKind for Type {
    fn kind(&self, cx: &Context) -> @Kind {
        match *self {
            TCon(tc) => tc.kind(cx),
            TVar(v) => v.kind(cx),
            TAp(t, _) => match *t.kind(cx) {
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
    fn kind(&self, cx: &Context) -> @Kind {
        cx.kinds.get_copy(&self.id)
    }
}

impl HasKind for Tyvar {
    fn kind(&self, cx: &Context) -> @Kind {
        cx.kinds.get_copy(&self.id)
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

impl cx::Describe for SubstPair {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        self.from.describe(cx, out);
        out.push_str(" -> ");
        self.to.describe(cx, out);
    }
}

impl cx::Describe for Subst {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        self.pairs.describe(cx, out);
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
            TAp(s, t) => util::union(s.tv(), t.tv()),
            TCon(_) | TGen(_) => ~[],
        }
    }
}

impl<A:Types> Types for ~[A] {
    fn apply(&self, s: &Subst) -> ~[A] {
        self.map(|a| a.apply(s))
    }
    fn tv(&self) -> ~[Tyvar] {
        self.iter().fold(~[], |v, a| util::union(v, a.tv()))
    }
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
