use ty;

pub enum Err {
    DuplicateBindings(ty::Id),
    MismatchedTypes(@ty::Type, @ty::Type),
    OccursCheck(ty::Tyvar, @ty::Type),
    KindCheck(ty::Tyvar, @ty::Type),
}

pub type Fallible<T> = Result<T,Err>;
