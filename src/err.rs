use ty;
use intern;

pub enum Err {
    DuplicateBindings(intern::Id),
    MismatchedTypes(@ty::Type, @ty::Type),
    OccursCheck(ty::Tyvar, @ty::Type),
    KindCheck(ty::Tyvar, @ty::Type),
}

pub type Fallible<T> = Result<T,Err>;
