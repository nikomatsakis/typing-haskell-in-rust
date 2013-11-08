use cx;
use cx::Context;
use ty;
use parse::*;

pub struct Grammar {
    kind: Parser<@ty::Kind>,
}

impl Grammar {
    pub fn new() -> Grammar {
        Grammar { kind: KindDef() }
    }
}

///////////////////////////////////////////////////////////////////////////

fn managed<T:'static>(t: T) -> @T { @t }

///////////////////////////////////////////////////////////////////////////
// Kind

struct Kind1;

fn Kind() -> Parser<@ty::Kind> {
    ~Kind1 as Parser<@ty::Kind>
}

impl Parse<@ty::Kind> for Kind1 {
    fn parse(&self,
             cx: &mut Context,
             input: &[u8],
             start: uint)
             -> ParseError<(uint, @ty::Kind)> {
        cx.grammar.kind.parse(cx, input, start)
    }
}

fn KindDef() -> Parser<@ty::Kind> {
    return Choice(~[ k1().then(Arrow().thenr(k1()).rep(1)).map(mk_kfun),
                     k1() ]);

    fn mk_star(cx: &mut Context, (): ()) -> @ty::Kind {
        cx.k_star
    }

    fn k1() -> Parser<@ty::Kind> {
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
