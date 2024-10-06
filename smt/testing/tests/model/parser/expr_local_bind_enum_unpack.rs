use rusmart_smt_remark_derive::{smt_impl, smt_type};
use rusmart_smt_stdlib::{Boolean, SMT};

#[smt_type]
enum E {
    V1(Boolean),
}

#[smt_impl]
fn foo(e: E) -> Boolean {
    let E::V1(x) = e;
    x
}
