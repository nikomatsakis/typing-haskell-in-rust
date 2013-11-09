use cx::{Context, Describe};
use intern::{Interner, Id};
use std::hashmap::HashMap;
use ty;
use unification::Unification;
use util;

/// preds => head
#[deriving(Eq)]
pub struct Qual<T> {
    preds: ~[Pred],
    head: T,
}

/// `ty` is a member of the type class `type_class`
#[deriving(Eq)]
pub struct Pred {
    type_class: Id,
    ty: @ty::Type,
}

pub struct ClassDecl {
    type_class: Id,
    superclasses: ~[Id],
}

pub struct Class {
    superclasses: ~[Id],
    instances: ~[Instance]
}

pub struct Instance {
    qual: Qual<Pred>
}

impl<T:ty::Types> ty::Types for Qual<T> {
    fn apply(&self, subst: &ty::Subst) -> Qual<T> {
        Qual { preds: self.preds.apply(subst),
               head: self.head.apply(subst) }
    }

    fn tv(&self) -> ~[ty::Tyvar] {
        util::union(self.preds.tv(), self.head.tv())
    }
}

impl ty::Types for Pred {
    fn apply(&self, subst: &ty::Subst) -> Pred {
        Pred { type_class: self.type_class,
               ty: self.ty.apply(subst) }
    }

    fn tv(&self) -> ~[ty::Tyvar] {
        self.ty.tv()
    }
}

pub struct ClassEnv {
    classes: HashMap<Id, Class>,
    defaults: ~[@ty::Type]
}

impl ClassEnv {
    pub fn new() -> ClassEnv {
        ClassEnv {
            classes: HashMap::new(),
            defaults: ~[]
        }
    }

    pub fn defined(&self, id: Id) -> bool {
        self.classes.contains_key(&id)
    }

    pub fn class<'a>(&'a self, id: Id) -> &'a Class {
        self.classes.get(&id)
    }

    pub fn mut_class<'a>(&'a mut self, id: Id) -> &'a mut Class {
        self.classes.find_mut(&id).unwrap()
    }

    pub fn superclasses<'a>(&'a self, id: Id) -> &'a [Id] {
        self.class(id).superclasses.slice_from(0)
    }

    pub fn instances<'a>(&'a self, id: Id) -> &'a [Instance] {
        self.class(id).instances.slice_from(0)
    }

    pub fn add_class_str(&mut self,
                         interner: &mut Interner,
                         id: &str,
                         superclasses: &[&str]) {
        let id = interner.id(id);
        let superclasses = superclasses.map(|&s| interner.id(s));
        self.add_class(interner, id, superclasses);
    }

    pub fn add_class(&mut self,
                     interner: &Interner,
                     id: Id,
                     superclasses: ~[Id]) {
        if self.defined(id) {
            fail!(format!("Already have a class named '{}'",
                          interner.to_str(id)));
        }

        match superclasses.iter().find(|&c| !self.defined(*c)) {
            Some(&c) => {
                fail!(format!("Superclass '{}' undefined",
                              interner.to_str(c)));
            }
            None => {}
        }

        let class = Class {
            superclasses: superclasses,
            instances: ~[]
        };

        self.classes.insert(id, class);
    }

    pub fn add_instance(&mut self,
                        cx: &Context,
                        preds: ~[Pred],
                        head: Pred) {
        if !self.defined(head.type_class) {
            fail!(format!("No type class named '{}'",
                          cx.interner.to_str(head.type_class)));
        }

        let overlapping = {
            let instances = self.instances(head.type_class);
            instances.iter().any(|i| self.overlap(cx, &head, &i.qual.head))
        };
        if overlapping {
            fail!(format!("Overlapping instance of '{}'",
                          cx.interner.to_str(head.type_class)));
        }

        let instance = Instance { qual: Qual { preds: preds, head: head } };
        self.mut_class(head.type_class).instances.push(instance);
    }

    fn overlap(&self, cx: &Context, head1: &Pred, head2: &Pred) -> bool {
        cx.mgu_pred(head1, head2).is_ok()
    }

    pub fn add_core_classes(&mut self, interner: &mut Interner) {
        self.add_class_str(interner, "Eq", []);
        self.add_class_str(interner, "Ord", ["Eq"]);
        self.add_class_str(interner, "Show", []);
        self.add_class_str(interner, "Bounded", []);
        self.add_class_str(interner, "Enum", []);
        self.add_class_str(interner, "Functor", []);
        self.add_class_str(interner, "Monad", []);

        self.add_class_str(interner, "Num", ["Eq", "Show"]);
        self.add_class_str(interner, "Real", ["Num", "Ord"]);
        self.add_class_str(interner, "Fractional", ["Num"]);
        self.add_class_str(interner, "Integral", ["Real", "Enum"]);
        self.add_class_str(interner, "RealFrac", ["Real", "Fractional"]);
        self.add_class_str(interner, "Floating", ["Fractional"]);
        self.add_class_str(interner, "RealFloat", ["RealFrac", "Floating"]);
    }
}

impl Describe for Pred {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        self.type_class.describe(cx, out);
        out.push_str(" ");
        self.ty.describe(cx, out);
    }
}

impl Describe for ClassDecl {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        out.push_str("class ");
        self.superclasses.describe(cx, out);
        out.push_str(" => ");
        self.type_class.describe(cx, out);
    }
}

impl Describe for Instance {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        out.push_str("instance ");
        self.qual.preds.describe(cx, out);
        out.push_str(" => ");
        self.qual.head.describe(cx, out);
    }
}
