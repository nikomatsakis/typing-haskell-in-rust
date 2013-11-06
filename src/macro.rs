#[macro_escape];

macro_rules! if_ok (
    ($expr:expr) => {
        match $expr {
            Ok(v) => v,
            Err(e) => { return Err(e); }
        }
    }
)

macro_rules! seq (
    ($(let $id:ident <- $v:expr;)+ return $r:expr;) => {
        { $(
            let $id = match $v {
                Ok(v) => v,
                Err(e) => { return Err(e); }
            };
        )+
        Ok($r) }
    }
)

