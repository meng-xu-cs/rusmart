use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, SMT};

#[smt_type]
enum E1 {
    None,
    Some { x: Boolean },
}

#[smt_impl]
fn f1(e: E1, x: Boolean) -> Boolean {
    match e {
        E1::None => Boolean::from(false),
        E1::Some { x } => x,
    }
}
