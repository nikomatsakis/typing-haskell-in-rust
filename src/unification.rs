use cx::Context;
use err;
use ty;
use ty::Types;
use ty::HasKind;

trait Unification {
    fn mgu(&mut self, t1: @ty::Type, t2: @ty::Type)
           -> err::Fallible<ty::Subst>;

    fn var_bind(&mut self, u: ty::Tyvar, t: @ty::Type)
                -> err::Fallible<ty::Subst>;

    fn match_types(&mut self, t1: @ty::Type, t2: @ty::Type)
                   -> err::Fallible<ty::Subst>;
}

impl Unification for Context {
    fn mgu(&mut self, t1: @ty::Type, t2: @ty::Type)
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

    fn var_bind(&mut self, u: ty::Tyvar, t: @ty::Type)
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

    fn match_types(&mut self, t1: @ty::Type, t2: @ty::Type)
                   -> err::Fallible<ty::Subst> {
        match (t1, t2) {
            (@ty::TAp(l, r), @ty::TAp(l1, r1)) => seq!(
                let s1 <- self.mgu(l, l1);
                let s2 <- self.mgu(r, r1);
                let r <- ty::merge(&s1, &s2);
                return r;
            ),

            (@ty::TVar(u), t) | (t, @ty::TVar(u)) => {
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
}
