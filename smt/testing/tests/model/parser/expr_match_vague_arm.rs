use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::{Boolean, SMT};

#[smt_type]
enum E {
    None,
    Some(Boolean),
}

#[smt_impl]
fn f(a: E, b: E) -> Boolean {
    match (a, b) {
        (E::None, _) => Boolean::from(false),
        (_, E::None) => Boolean::from(false),
        (E::Some(v1), E::Some(v2)) => v1.xor(v2),
    }
}
