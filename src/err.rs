use cx;
use cx::Describe;
use ty;
use tc = type_class;
use intern;

pub enum Err {
    DuplicateBindings(intern::Id),
    MismatchedTypes(@ty::Type, @ty::Type),
    OccursCheck(ty::Tyvar, @ty::Type),
    ClassesDiffer(intern::Id, intern::Id),
    KindCheck(ty::Tyvar, @ty::Type),
    NoInstance(tc::Pred)
}

pub type Fallible<T> = Result<T,Err>;

#[cfg(test)]
pub fn check_ok<T>(e: Fallible<T>) -> T {
    match e {
        Ok(v) => v,
        Err(e) => {
            fail!(format!("Unexpected error: {:?}", e))
        }
    }
}

#[cfg(test)]
pub fn check_err<T:Describe>(cx: &mut cx::Context,
                             exp: &'static str,
                             e: Fallible<T>) {
    match e {
        Err(e) => {
            let d = format!("{:?}", e);
            if !d.contains(exp) {
                fail!(format!("Error string {} does not include {}",
                              d, exp));
            }
        }
        Ok(v) => {
            fail!(format!("Unexpected success: {}", cx.mk_str(v)));
        }
    }
}

impl ToStr for Err {
    fn to_str(&self) -> ~str {
        format!("{:?}", *self)
    }
}
