// Context: ties everything together

use grammar;
use intern;
use intern::Id;
use parse;
use std::hashmap::HashMap;
use ty;

pub struct Context {
    interner: intern::Interner,
    k_star: @ty::Kind,
    types: StandardTypes,
    kinds: HashMap<Id,@ty::Kind>,
}

pub struct StandardTypes {
    t_unit: @ty::Type,
    t_char: @ty::Type,
    t_int: @ty::Type,
    t_integer: @ty::Type,
    t_float: @ty::Type,
    t_double: @ty::Type,
    t_list: @ty::Type,
    t_arrow: @ty::Type,
    t_tuple2: @ty::Type,
}

impl Context {
    pub fn new() -> Context {
        let mut interner = intern::Interner::new();

        let k_star = @ty::Star;

        let types = StandardTypes {
            t_unit: @ty::TCon(ty::Tycon { id: interner.id("()") }),
            t_char: @ty::TCon(ty::Tycon { id: interner.id("Char") }),
            t_int: @ty::TCon(ty::Tycon { id: interner.id("Int") }),
            t_integer: @ty::TCon(ty::Tycon { id: interner.id("Integer") }),
            t_float: @ty::TCon(ty::Tycon { id: interner.id("Float") }),
            t_double: @ty::TCon(ty::Tycon { id: interner.id("Double") }),

            t_list: @ty::TCon(ty::Tycon { id: interner.id("[]") }),
            t_arrow: @ty::TCon(ty::Tycon { id: interner.id("(->)") }),
            t_tuple2: @ty::TCon(ty::Tycon { id: interner.id("(,)") }),
        };

        let mut kinds = HashMap::new();
        kinds.insert(interner.id("()"), k_star);
        kinds.insert(interner.id("Char"), k_star);
        kinds.insert(interner.id("Int"), k_star);
        kinds.insert(interner.id("Integer"), k_star);
        kinds.insert(interner.id("Float"), k_star);
        kinds.insert(interner.id("Double"), k_star);
        kinds.insert(interner.id("[]"), @ty::KFun(k_star, k_star));
        kinds.insert(interner.id("(->)"),
                     @ty::KFun(k_star, @ty::KFun(k_star, k_star)));
        kinds.insert(interner.id("(,)"),
                     @ty::KFun(k_star, @ty::KFun(k_star, k_star)));

        Context {
            interner: interner,
            k_star: k_star,
            types: types,
            kinds: HashMap::new(),
        }
    }

    pub fn id(&mut self, nm: &str) -> Id {
        self.interner.id(nm)
    }

    pub fn func(&self, input: @ty::Type, output: @ty::Type) -> @ty::Type {
        @ty::TAp(@ty::TAp(self.types.t_arrow, input), output)
    }

    pub fn list(&self, elem: @ty::Type) -> @ty::Type {
        @ty::TAp(self.types.t_list, elem)
    }

    pub fn pair(&self, a: @ty::Type, b: @ty::Type) -> @ty::Type {
        @ty::TAp(@ty::TAp(self.types.t_tuple2, a), b)
    }

    pub fn load_kind_defs(&mut self, defs: &[&str]) {
        let g = grammar::Grammar::new();
        let kind_def_parser = grammar::KindDef();
        for text in defs.iter() {
            let kind_def = parse::parse_or_fail(&g, self, text.as_bytes(),
                                                &kind_def_parser);
            self.kinds.insert(kind_def.id, kind_def.kind);
        }
    }

    pub fn parse_ty(&mut self, text: &str) -> @ty::Type {
        let g = grammar::Grammar::new();
        parse::parse_or_fail(&g, self, text.as_bytes(), &g.ty)
    }

    pub fn mk_str<D:Describe>(&self, d: D) -> ~str {
        let mut s = ~"";
        d.describe(self, &mut s);
        s
    }
}


pub trait Describe {
    fn describe(&self, cx: &Context, out: &mut ~str);
}

impl Describe for () {
    fn describe(&self, _: &Context, out: &mut ~str) {
        out.push_str("()")
    }
}

impl Describe for uint {
    fn describe(&self, _: &Context, out: &mut ~str) {
        out.push_str(format!("{}", *self));
    }
}

impl<T:Describe> Describe for @T {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        let &@ref this = self;
        this.describe(cx, out);
    }
}

impl<D:Describe> Describe for ~[D] {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        let mut comma = false;
        out.push_char('[');
        for item in self.iter() {
            if comma { out.push_char(','); }
            item.describe(cx, out);
            comma = true;
        }
        out.push_char(']');
    }
}

impl<D:Describe,E:Describe> Describe for (D,E) {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        let (ref d, ref e) = *self;

        out.push_char('(');
        d.describe(cx, out);
        out.push_char(',');
        e.describe(cx, out);
        out.push_char(')');
    }
}



