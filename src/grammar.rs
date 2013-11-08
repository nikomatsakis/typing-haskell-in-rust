use cx::Context;
use ty;
use parse::*;

pub struct Grammar {
    kind: GParser<@ty::Kind>,
}

impl Grammar {
    pub fn new() -> Grammar {
        Grammar { kind: KindDef() }
    }
}

type GParser<T> = Parser<Grammar, T>;

///////////////////////////////////////////////////////////////////////////

fn managed<T:'static>(t: T) -> @T { @t }

///////////////////////////////////////////////////////////////////////////
// Kind

fn Kind() -> GParser<@ty::Kind> {
    return Ref(get_kind);

    fn get_kind<'a>(g: &'a Grammar) -> &'a GParser<@ty::Kind> {
        &g.kind
    }
}

fn KindDef() -> GParser<@ty::Kind> {
    return Choice(~[ k1().then(Arrow().thenr(k1()).rep(1)).map(mk_kfun),
                     k1() ]);

    fn mk_star(cx: &mut Context, (): ()) -> @ty::Kind {
        cx.k_star
    }

    fn k1() -> GParser<@ty::Kind> {
        Choice(~[ Star().map_cx(mk_star),
                  Lparen().thenr(Kind()).thenl(Rparen()) ])
    }

    fn mk_kfun((k_0, ks): (@ty::Kind, ~[@ty::Kind])) -> @ty::Kind {
        // k1 -> k2 -> k3 -> k4 should become:
        //
        //       ->
        //  k1       ->
        //        k2   ->
        //           k3  k4

        let ks_n = *ks.last();
        let ks_1 = ks.slice(0, ks.len() - 1);
        let k_1 = ks_1.rev_iter().fold(ks_n, |k1, &k2| @ty::KFun(k1, k2));
        @ty::KFun(k_0, k_1)
    }
}

#[test]
fn parse_kind() {
    let k = Kind();
    test(Grammar::new(), "*", &k, "*");
    test(Grammar::new(), "* -> * -> *", &k, "(* -> (* -> *))");
    test(Grammar::new(), "(* -> *) -> *", &k, "((* -> *) -> *)");
}
