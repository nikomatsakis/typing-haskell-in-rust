use intern::{Interner, Id};
use std::hashmap::HashMap;
use ty;
use util;

/// preds => then
#[deriving(Eq)]
pub struct Qual<T> {
    preds: ~[Pred],
    then: T,
}

/// `ty` is a member of the type class `type_class`
#[deriving(Eq)]
pub struct Pred {
    type_class: Id,
    ty: @ty::Type,
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
               then: self.then.apply(subst) }
    }

    fn tv(&self) -> ~[ty::Tyvar] {
        util::union(self.preds.tv(), self.then.tv())
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
