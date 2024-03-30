use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, SMT};

#[smt_type]
enum E1 {
    None,
    Some(Boolean),
}

#[smt_impl]
fn f1(e: E1) -> Boolean {
    match e {
        E1::None => Boolean::from(false),
        E1::Some(v) => v,
    }
}

#[smt_impl]
fn f2(a: E1, b: E1) -> Boolean {
    match (a, b) {
        (E1::None, E1::None) => Boolean::from(false),
        (E1::None, _) => Boolean::from(false),
        (_, E1::None) => Boolean::from(false),
        (E1::Some(v1), E1::Some(v2)) => v1.xor(v2),
    }
}
