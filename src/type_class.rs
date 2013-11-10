use cx::{Context, Describe};
use err;
use err::Fallible;
use intern::{Interner, Id};
use std::hashmap::HashMap;
use std::result;
use std::vec;
use ty;
use ty::Types;
use unification::Unification;
use util;

/// preds => head
#[deriving(Eq)]
pub struct Qual<T> {
    preds: ~[Pred],
    head: T,
}

/// `ty` is a member of the type class `type_class`
#[deriving(Eq,Clone)]
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

impl Pred {
    pub fn is_head_normal_form(&self) -> bool {
        self.ty.is_head_normal_form()
    }
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

    pub fn add_instance_str(&mut self,
                            cx: &mut Context,
                            preds_str: ~[&str],
                            head_str: &str) {
        let preds = preds_str.map(|&p| cx.parse_pred(p));
        let head = cx.parse_pred(head_str);
        self.add_instance(cx, preds, head);
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

    pub fn add_core_instances(&mut self, cx: &mut Context) {
        self.add_instance_str(cx, ~[], "Eq Unit");
        self.add_instance_str(cx, ~[], "Eq Char");
        self.add_instance_str(cx, ~[], "Eq Int");

        self.add_instance_str(cx, ~["Eq a", "Eq b"], "Eq (Pair a b)");
        self.add_instance_str(cx, ~["Eq a"], "Eq (List a)");

        self.add_instance_str(cx, ~[], "Ord Unit");
        self.add_instance_str(cx, ~[], "Ord Char");
        self.add_instance_str(cx, ~[], "Ord Int");

        self.add_instance_str(cx, ~["Ord a", "Ord b"], "Ord (Pair a b)");
        self.add_instance_str(cx, ~["Ord a"], "Ord (List a)");
    }

    pub fn add_core(&mut self, cx: &mut Context) {
        self.add_core_classes(&mut cx.interner);
        self.add_core_instances(cx);
    }

    pub fn by_super(&self, head: Pred) -> ~[Pred] {
        /*!
         * Returns a list of type class relations that must hold if head holds,
         * due to the superclass relationship.
         */

        let mut result = ~[head];
        for &id in self.superclasses(head.type_class).iter() {
            result.push_all_move(self.by_super(Pred {type_class: id,
                                                     ty: head.ty}));
        }
        result
    }

    pub fn by_instance(&self,
                       cx: &Context,
                       head: Pred)
                       -> Fallible<~[Pred]> {
        /*!
         * Returns a list of type class relations that must hold if head holds,
         * due to the predicates attached to instance declarations.
         */

        for candidate in self.instances(head.type_class).iter() {
            match cx.match_pred(&candidate.qual.head, &head) {
                Ok(subst) => {
                    // Because we do not permit overlapping instances,
                    // at most one instance should match.
                    let result = candidate.qual.preds.apply(&subst);
                    debug!("by_instance head={} result={}",
                           cx.mk_str(head), cx.mk_str(&result));
                    return Ok(result);
                }
                Err(e) => {
                    debug!("error: {:?}", e);
                }
            }
        }
        debug!("by_instance head={} result=None",
               cx.mk_str(head));
        Err(err::NoInstance(head))
    }

    pub fn entail(&self, cx: &Context, preds: &[Pred], head: Pred) -> bool {
        // This algorithm seems incomplete to me. The paper claims that it
        // computes whether knowing that `preds` hold implies that `head`
        // holds. But there are cases where `entail` returns false
        // when it seems like it ought to return true. For example,
        //
        //     Ord (Pair Foo Bar) `entail` Eq Foo == False
        //
        // but `Ord (Pair Foo Bar)` implies (by instance) that
        // `Ord Foo` and `Ord Bar` and `Ord Foo` implies (by superclass)
        // `Eq Foo`.
        //
        // I think ultimately the reason for this is that it is assumed
        // (though not stated in the paper) that `preds` are in
        // head-normal-form.

        // Is `head` implied directly by something in `preds`?
        debug!("entail: preds={} head={}",
               cx.mk_str(preds),
               cx.mk_str(head));
        preds.iter().any(|&pred| self.by_super(pred).contains(&head)) || {
            // If not, is there an instance where `preds` satisfies all
            // the preconditions of the instance?
            match self.by_instance(cx, head) {
                Ok(heads) => heads.iter().all(|&h| self.entail(cx, preds, h)),
                Err(_) => false
            }
        }
    }

    fn pred_to_head_normal_form(&self,
                                cx: &Context,
                                p: Pred)
                                -> Fallible<~[Pred]> {
        if p.is_head_normal_form() {
            Ok(~[p])
        } else {
            self.by_instance(cx, p).and_then(
                |ps| self.preds_to_head_normal_form(cx, ps))
        }
    }

    fn preds_to_head_normal_form(&self,
                                 cx: &Context,
                                 ps: &[Pred])
                                 -> Fallible<~[Pred]> {
        seq!(
            let ps <- result::collect(ps.iter().map(
                    |&p| self.pred_to_head_normal_form(cx, p)));
            return ps.move_iter().flat_map(|v| v.move_iter()).collect();
        )
    }

    fn simplify(&self,
                cx: &Context,
                preds: &[Pred])
                -> ~[Pred] {
        debug!("simplify preds={}", cx.mk_str(preds));
        let mut result = ~[];
        for i in range(0, preds.len()) {
            let the_rest = vec::append(result.clone(),
                                       preds.slice_from(i+1));
            if !self.entail(cx, the_rest, preds[i]) {
                result.push(preds[i]);
            }
        }
        debug!("simplify preds={} result={}",
               cx.mk_str(preds), cx.mk_str(&result));
        result
    }

    fn reduce(&self,
              cx: &Context,
              preds: &[Pred])
              -> Fallible<~[Pred]> {
        seq!(
            let hnfs <- self.preds_to_head_normal_form(cx, preds);
            return self.simplify(cx, hnfs);
        )
    }
}

impl<T:Describe> Describe for Qual<T> {
    fn describe(&self, cx: &Context, out: &mut ~str) {
        self.preds.describe(cx, out);
        out.push_str(" => ");
        self.head.describe(cx, out);
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

#[cfg(test)]
fn init_env() -> (Context, ClassEnv) {
    let mut cx = Context::new();
    let mut env = ClassEnv::new();
    env.add_core(&mut cx);
    (cx, env)
}

#[test]
fn by_super() {
    let (mut cx, env) = init_env();
    let pred1 = cx.parse_pred("Ord Int");
    let preds = env.by_super(pred1);
    assert_eq!(cx.mk_str(preds), ~"[Ord Int,Eq Int]");
}

#[test]
fn by_super_pair() {
    let (mut cx, env) = init_env();
    cx.load_kind_defs(["Foo :: *", "Bar :: *"]);
    let pred1 = cx.parse_pred("Ord (Pair Foo Bar)");
    let preds = env.by_super(pred1);
    assert_eq!(cx.mk_str(preds), ~"[Ord ((Pair Foo) Bar),Eq ((Pair Foo) Bar)]");
}

#[test]
fn by_instance() {
    let (mut cx, env) = init_env();
    cx.load_kind_defs(["Foo :: *", "Bar :: *"]);
    let pred1 = cx.parse_pred("Ord (Pair Foo Bar)");
    let preds = env.by_instance(&cx, pred1);
    assert_eq!(cx.mk_str(preds), ~"[Ord Foo,Ord Bar]");
}

#[test]
fn by_instance_none() {
    let (mut cx, env) = init_env();
    cx.load_kind_defs(["Foo :: *", "Bar :: *"]);
    let pred1 = cx.parse_pred("Ord Foo");
    let preds = env.by_instance(&cx, pred1);
    assert_eq!(cx.mk_str(preds), ~"Err");
}

#[test]
fn entail1() {
    let (mut cx, env) = init_env();
    cx.load_kind_defs(["Foo :: *", "Bar :: *"]);
    let pred1 = cx.parse_pred("Ord (Pair Foo Bar)");
    let pred2 = cx.parse_pred("Ord Foo");
    let pred3 = cx.parse_pred("Ord Bar");
    assert!(env.entail(&cx, [pred2,pred3], pred1));
}

#[test]
fn to_hnf() {
    let (mut cx, env) = init_env();
    cx.load_kind_defs(["Foo :: *", "Bar :: *"]);
    let pred1 = cx.parse_pred("Ord (Pair a b)");
    let hnfs = env.preds_to_head_normal_form(&cx, [pred1]);
    assert_eq!(cx.mk_str(hnfs), ~"[Ord a,Ord b]");
}

#[test]
fn reduce_1() {
    let (mut cx, env) = init_env();
    cx.load_kind_defs(["Foo :: *", "Bar :: *"]);
    let pred1 = cx.parse_pred("Ord (Pair a b)");
    let r = env.reduce(&cx, [pred1]);
    assert_eq!(cx.mk_str(r), ~"[Ord a,Ord b]");
}

#[test]
fn reduce_2() {
    let (mut cx, env) = init_env();
    cx.load_kind_defs(["Foo :: *", "Bar :: *"]);
    let pred1 = cx.parse_pred("Ord (Pair a b)");
    let pred2 = cx.parse_pred("Eq (Pair a b)");
    let pred3 = cx.parse_pred("Ord (List a)");
    let r = env.reduce(&cx, [pred1, pred2, pred3]);
    assert_eq!(cx.mk_str(r), ~"[Ord b,Ord a]");
}
