use cx::Context;
use err;
use ty;
use ty::Types;
use ty::HasKind;
use tc = type_class;

pub trait Unification {
    fn mgu(&self, t1: @ty::Type, t2: @ty::Type)
           -> err::Fallible<ty::Subst>;

    fn mgu_pred(&self, t1: &tc::Pred, t2: &tc::Pred)
                -> err::Fallible<ty::Subst>;

    fn var_bind(&self, u: ty::Tyvar, t: @ty::Type)
                -> err::Fallible<ty::Subst>;

    fn match_types(&self, t1: @ty::Type, t2: @ty::Type)
                   -> err::Fallible<ty::Subst>;

    fn match_pred(&self, t1: &tc::Pred, t2: &tc::Pred)
                  -> err::Fallible<ty::Subst>;
}

impl Unification for Context {
    fn mgu(&self, t1: @ty::Type, t2: @ty::Type)
           -> err::Fallible<ty::Subst> {
        match (t1, t2) {
            (@ty::TAp(l, r), @ty::TAp(l1, r1)) => seq!(
                let s1 <- self.mgu(l, l1);
                let s2 <- self.mgu(r, r1);
                let r <- ty::merge(&s1, &s2);
                return r;
            ),

            (@ty::TVar(u), t) |
                (t, @ty::TVar(u)) => {
                self.var_bind(u, t)
            }

            (@ty::TCon(tc1), @ty::TCon(tc2)) => {
                if tc1 == tc2 {
                    Ok(ty::Subst::null())
                } else {
                    Err(err::MismatchedTypes(t1, t2))
                }
            }

            _ => {
                Err(err::MismatchedTypes(t1, t2))
            }
        }
    }

    fn mgu_pred(&self, p1: &tc::Pred, p2: &tc::Pred)
                -> err::Fallible<ty::Subst> {
        if p1.type_class == p2.type_class {
            self.mgu(p1.ty, p2.ty)
        } else {
            Err(err::ClassesDiffer(p1.type_class, p2.type_class))
        }
    }

    fn var_bind(&self, u: ty::Tyvar, t: @ty::Type)
                -> err::Fallible<ty::Subst> {
        if ty::TVar(u) == *t {
            Ok(ty::Subst::null())
        } else if t.tv().contains(&u) {
            Err(err::OccursCheck(u, t))
        } else if u.kind(self) != t.kind(self) {
            Err(err::KindCheck(u, t))
        } else {
            Ok(ty::Subst::from(u, t))
        }
    }

    fn match_types(&self, t1: @ty::Type, t2: @ty::Type)
                   -> err::Fallible<ty::Subst> {
        debug!("match_types(t1={}, t2={})",
               self.mk_str(t1),
               self.mk_str(t2));
        match (t1, t2) {
            (@ty::TAp(l, r), @ty::TAp(l1, r1)) => seq!(
                let s1 <- self.match_types(l, l1);
                let s2 <- self.match_types(r, r1);
                let r <- ty::merge(&s1, &s2);
                return r;
            ),

            (@ty::TVar(u), t) if u.kind(self) == t.kind(self) => {
                Ok(ty::Subst::from(u, t))
            }

            (@ty::TCon(tc1), @ty::TCon(tc2)) => {
                if tc1 == tc2 {
                    Ok(ty::Subst::null())
                } else {
                    Err(err::MismatchedTypes(t1, t2))
                }
            }

            _ => {
                Err(err::MismatchedTypes(t1, t2))
            }
        }
    }

    fn match_pred(&self, p1: &tc::Pred, p2: &tc::Pred)
                  -> err::Fallible<ty::Subst> {
        if p1.type_class == p2.type_class {
            self.match_types(p1.ty, p2.ty)
        } else {
            Err(err::ClassesDiffer(p1.type_class, p2.type_class))
        }
    }
}

#[test]
fn test_mgu_very_simple() {
    let mut cx = Context::new();
    cx.load_kind_defs(["List :: * -> *",
                       "Int :: *",
                       "a :: *"]);
    let t1 = cx.parse_ty("List Int");
    let t2 = cx.parse_ty("List a");
    let s = err::check_ok(cx.mgu(t1, t2));
    assert_eq!(cx.mk_str(s), ~"[a -> Int]");
}

#[test]
fn test_mgu_bad_kind() {
    let mut cx = Context::new();
    cx.load_kind_defs(["List :: * -> *",
                       "Int :: *",
                       "a :: * -> *"]);
    let t1 = cx.parse_ty("List Int");
    let t2 = cx.parse_ty("List a");
    let r = cx.mgu(t1, t2);
    err::check_err(&mut cx, "KindCheck", r);
}

#[test]
fn test_mgu_bad_occurs() {
    let mut cx = Context::new();
    cx.load_kind_defs(["List :: * -> *",
                       "Int :: *",
                       "a :: * -> *"]);
    let t1 = cx.parse_ty("List (List a)");
    let t2 = cx.parse_ty("List a");
    let r = cx.mgu(t1, t2);
    err::check_err(&mut cx, "OccursCheck", r);
}

#[test]
fn test_match_right_way() {
    let mut cx = Context::new();
    cx.load_kind_defs(["List :: * -> *",
                       "Int :: *",
                       "a :: *"]);
    let t1 = cx.parse_ty("List a");
    let t2 = cx.parse_ty("List Int");
    let r = err::check_ok(cx.match_types(t1, t2));
    assert_eq!(cx.mk_str(r), ~"[a -> Int]");
}

#[test]
fn test_match_wrong_way() {
    let mut cx = Context::new();
    cx.load_kind_defs(["List :: * -> *",
                       "Int :: *",
                       "a :: *"]);
    let t1 = cx.parse_ty("List Int");
    let t2 = cx.parse_ty("List a");
    let r = cx.match_types(t1, t2);
    err::check_err(&mut cx, "MismatchedTypes", r);
}
