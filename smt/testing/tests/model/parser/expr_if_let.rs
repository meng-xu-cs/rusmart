use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::{Boolean, Integer, SMT};

#[smt_type]
enum E {
    None,
    Some(Boolean),
}

#[smt_impl]
fn f(e: E) -> Boolean {
    if let E::Some(x) = e {
        x
    } else {
        Boolean::from(false)
    }
}
