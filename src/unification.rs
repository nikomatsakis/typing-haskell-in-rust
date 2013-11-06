use err;
use ty;
use ty::Types;
use ty::HasKind;

fn mgu(t1: @ty::Type, t2: @ty::Type) -> err::Fallible<ty::Subst> {
    match (t1, t2) {
        (@ty::TAp(l, r), @ty::TAp(l1, r1)) => seq!(
            let s1 <- mgu(l, l1);
            let s2 <- mgu(r, r1);
            let r <- ty::merge(&s1, &s2);
            return r;
        ),

        (@ty::TVar(u), t) |
        (t, @ty::TVar(u)) => {
            varBind(u, t)
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

fn varBind(u: ty::Tyvar, t: @ty::Type) -> err::Fallible<ty::Subst> {
    if ty::TVar(u) == *t {
        Ok(ty::Subst::null())
    } else if t.tv().contains(&u) {
        Err(err::OccursCheck(u, t))
    } else if u.kind != t.kind() {
        Err(err::KindCheck(u, t))
    } else {
        Ok(ty::Subst::from(u, t))
    }
}

fn match_types(t1: @ty::Type, t2: @ty::Type) -> err::Fallible<ty::Subst> {
    match (t1, t2) {
        (@ty::TAp(l, r), @ty::TAp(l1, r1)) => seq!(
            let s1 <- mgu(l, l1);
            let s2 <- mgu(r, r1);
            let r <- ty::merge(&s1, &s2);
            return r;
        ),

        (@ty::TVar(u), t) |
        (t, @ty::TVar(u)) => {
            varBind(u, t)
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
