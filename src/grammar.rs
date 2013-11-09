use cx::Context;
use intern::Id;
use ty;
use tc = type_class;
use parse::*;

pub struct Grammar {
    kind: GParser<@ty::Kind>,
    ty: GParser<@ty::Type>,
}

impl Grammar {
    pub fn new() -> Grammar {
        Grammar { kind: MkKind(),
                  ty: MkTy() }
    }
}

type GParser<T> = Parser<Grammar, T>;

///////////////////////////////////////////////////////////////////////////

fn managed<T:'static>(t: T) -> @T { @t }

///////////////////////////////////////////////////////////////////////////
// Kind

fn Kind() -> GParser<@ty::Kind> {
    return Ref(get);

    fn get<'a>(g: &'a Grammar) -> &'a GParser<@ty::Kind> {
        &g.kind
    }
}

fn MkKind() -> GParser<@ty::Kind> {
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
        let k_1 = ks_1.rev_iter().fold(ks_n, |k1, &k2| @ty::KFun(k2, k1));
        @ty::KFun(k_0, k_1)
    }
}

#[test]
fn parse_kind_star() {
    let k = Kind();
    test(Grammar::new(), "*", &k, "*");
}

#[test]
fn parse_kind_star_arrow_star_arrow_star() {
    let k = Kind();
    test(Grammar::new(), "* -> * -> *", &k, "(* -> (* -> *))");
}

#[test]
fn parse_kind_paren_l() {
    let k = Kind();
    test(Grammar::new(), "(* -> *) -> *", &k, "((* -> *) -> *)");
}

#[test]
fn parse_kind_paren_r() {
    let k = Kind();
    test(Grammar::new(), "* -> * -> (* -> *)", &k, "(* -> (* -> (* -> *)))");
}

///////////////////////////////////////////////////////////////////////////
// KindDef

pub fn KindDef() -> GParser<ty::KindDef> {
    let c = Choice(~[
            Ident().thenl(ColonColon()).then(Kind()).thenl(Semi()),
            TypeName().thenl(ColonColon()).then(Kind()).thenl(Semi()) ]);
    return c.map(mk);

    fn mk((id, kind): (Id, @ty::Kind)) -> ty::KindDef {
        ty::KindDef { id: id, kind: kind }
    }
}

#[test]
fn parse_kind_def() {
    let k = KindDef().rep(1);
    test(Grammar::new(),
         "a :: * -> *; Box :: *;",
         &k,
         "[a :: (* -> *),Box :: *]");
}

///////////////////////////////////////////////////////////////////////////
// Type

fn Ty() -> GParser<@ty::Type> {
    return Ref(get);

    fn get<'a>(g: &'a Grammar) -> &'a GParser<@ty::Type> {
        &g.ty
    }
}

fn MkTy() -> GParser<@ty::Type> {
    return t1().rep(1).map(mk_ap);

    fn t1() -> GParser<@ty::Type> {
        Choice(~[ Ident().map(mk_tvar),
                  TypeName().map(mk_tcon),
                  Integer().map(mk_gen),
                  Lparen().thenr(Ty()).thenl(Rparen()), ])
    }

    fn mk_tvar(id: Id) -> @ty::Type {
        @ty::TVar(ty::Tyvar {id: id})
    }

    fn mk_tcon(id: Id) -> @ty::Type {
        @ty::TCon(ty::Tycon {id: id})
    }

    fn mk_gen(i: uint) -> @ty::Type {
        @ty::TGen(i)
    }

    fn mk_ap(tys: ~[@ty::Type]) -> @ty::Type {
        // Vector List Int ==>
        //
        //    ((Vector List) Int)
        let first = tys[0];
        let rest = tys.slice_from(1);
        rest.iter().fold(first, |t1, &t2| @ty::TAp(t1, t2))
    }
}

#[test]
fn parse_ty_a() {
    let k = Ty();
    test(Grammar::new(), "List a", &k, "(List 'a)")
}

#[test]
fn parse_ty_3() {
    let k = Ty();
    test(Grammar::new(), "Vector List Int", &k, "((Vector List) Int)")
}

#[test]
fn parse_ty_paren() {
    let k = Ty();
    test(Grammar::new(), "Vector (List Int) Foo", &k, "((Vector (List Int)) Foo)")
}

///////////////////////////////////////////////////////////////////////////
// Predicates
//
// TypeClass type ==> e.g., Eq Int

fn Pred() -> GParser<tc::Pred> {
    return TypeName().then(Ty()).map(mk_pred);

    fn mk_pred((type_class, ty): (Id, @ty::Type)) -> tc::Pred {
        tc::Pred { type_class: type_class, ty: ty }
    }
}

#[test]
fn parse_pred() {
    let k = Pred();
    test(Grammar::new(), "Eq Int", &k, "Eq Int")
}

///////////////////////////////////////////////////////////////////////////
// Type Class declaration
//
// In real Haskell, we might write:
//
//   class C a, D a => E a where ...
//
// But in this limited version, we don't support MPTC, don't
// really care about the specific methods, and don't want to
// validate the rules for legal heads/instances. Therefore,
// we just write:
//
//   class C D => E

fn ClassDecl() -> GParser<tc::ClassDecl> {
    return (Class().
            thenr(TypeName().rep(0).thenl(FatArrow()).opt()).
            then(TypeName())).map(mk_tc);

    fn mk_tc((superclasses, head): (Option<~[Id]>, Id)) -> tc::ClassDecl {
        let superclasses = match superclasses { Some(v) => v, None => ~[] };
        tc::ClassDecl { type_class: head, superclasses: superclasses }
    }
}


#[test]
fn parse_classDecl_no_super() {
    let k = ClassDecl();
    test(Grammar::new(), "class Eq", &k, "class [] => Eq")
}

#[test]
fn parse_classDecl_one_super() {
    let k = ClassDecl();
    test(Grammar::new(), "class Eq => Ord", &k, "class [Eq] => Ord")
}

#[test]
fn parse_classDecl_two_super() {
    let k = ClassDecl();
    test(Grammar::new(), "class Foo Bar => Ord", &k, "class [Foo,Bar] => Ord")
}

