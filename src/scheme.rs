/*!
 * Type schemes
 */

use cx::{Context, Describe};
use ty;
use ty::Types;
use ty::HasKind;
use tc = type_class;

#[deriving(Eq)]
struct Scheme {
    // Kinds for each of the generic types
    kinds: ~[@ty::Kind],

    // Qualifier which may include references to TGen(i) where
    // 0 <= i < self.kinds.len()
    qual: tc::Qual<@ty::Type>,
}

impl Describe for Scheme {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        out.push_str("(Forall ");
        self.kinds.describe(cx, out);
        out.push_str(". ");
        self.qual.describe(cx, out);
        out.push_str(")");
    }
}

impl Types for Scheme {
    fn apply(&self, subst: &ty::Subst) -> Scheme {
        Scheme {
            kinds: self.kinds.clone(),
            qual: self.qual.apply(subst),
        }
    }
    fn tv(&self) -> ~[ty::Tyvar] {
        self.qual.tv()
    }
}

pub trait Quantify {
    fn quantify(&self,
                variables0: &[ty::Tyvar],
                qual: &tc::Qual<@ty::Type>)
                -> Scheme;
}

impl Quantify for Context {
    fn quantify(&self,
                variables0: &[ty::Tyvar],
                qual: &tc::Qual<@ty::Type>)
                -> Scheme {
        let qual_tv = qual.tv();
        let variables: ~[ty::Tyvar] =
            qual_tv.iter().
            map(|&v| v).
            filter(|v| variables0.contains(v)).
            collect();
        let kinds = variables.map(|v| v.kind(self));
        let subst = ty::Subst {
            pairs: variables.iter().enumerate().map(
                |(i, &v)| ty::SubstPair { from: v, to: @ty::TGen(i) }).collect()
        };
        Scheme { kinds: kinds,
                 qual: qual.apply(&subst) }
    }
}

pub fn to_scheme(t: @ty::Type) -> Scheme {
    Scheme { kinds: ~[], qual: tc::Qual { preds: ~[], head: t } }
}

#[test]
fn mk_scheme() {
    let mut cx = Context::new();
    let qty = cx.parse_qual_ty("Eq a => Pair (List b) (List a)");
    let var_a = cx.parse_tyvar("a");
    let var_b = cx.parse_tyvar("b");

    let q1 = cx.quantify([var_a, var_b], &qty);
    assert_eq!(cx.mk_str(&q1), ~"(Forall [*,*]. [Eq '0] => ((Pair (List '1)) (List '0)))");

    // Doesn't matter what order we pass in the variables to quantify over:
    let q2 = cx.quantify([var_b, var_a], &qty);
    assert_eq!(q1, q2);
}
