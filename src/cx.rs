// Context: ties everything together

use ty;
use intern;

pub struct Context {
    interner: intern::Interner,
    k_star: @ty::Kind,
    types: StandardTypes,
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
            t_unit: @ty::TCon(ty::Tycon { id: interner.id("()"), kind: k_star }),
            t_char: @ty::TCon(ty::Tycon { id: interner.id("Char"), kind: k_star }),
            t_int: @ty::TCon(ty::Tycon { id: interner.id("Int"), kind: k_star }),
            t_integer: @ty::TCon(ty::Tycon { id: interner.id("Integer"), kind: k_star }),
            t_float: @ty::TCon(ty::Tycon { id: interner.id("Float"), kind: k_star }),
            t_double: @ty::TCon(ty::Tycon { id: interner.id("Double"), kind: k_star }),

            t_list: @ty::TCon(ty::Tycon { id: interner.id("[]"), kind: @ty::KFun(k_star, k_star) }),
            t_arrow: @ty::TCon(ty::Tycon { id: interner.id("(->)"), kind: @ty::KFun(k_star, @ty::KFun(k_star, k_star)) }),
            t_tuple2: @ty::TCon(ty::Tycon { id: interner.id("(,)"), kind: @ty::KFun(k_star, @ty::KFun(k_star, k_star)) }),
        };

        Context {
            interner: interner,
            k_star: k_star,
            types: types,
        }
    }

    fn func(&self, input: @ty::Type, output: @ty::Type) -> @ty::Type {
        @ty::TAp(@ty::TAp(self.types.t_arrow, input), output)
    }

    fn list(&self, elem: @ty::Type) -> @ty::Type {
        @ty::TAp(self.types.t_list, elem)
    }

    fn pair(&self, a: @ty::Type, b: @ty::Type) -> @ty::Type {
        @ty::TAp(@ty::TAp(self.types.t_tuple2, a), b)
    }
}


pub trait Describe {
    fn mk_str(&self, cx: &Context) -> ~str {
        let mut s = ~"";
        self.describe(cx, &mut s);
        s
    }

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



