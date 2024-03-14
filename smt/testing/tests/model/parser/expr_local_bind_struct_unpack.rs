use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, SMT};

#[smt_type]
struct S {
    f: Boolean,
}

#[smt_impl]
fn foo(s: S) -> Boolean {
    let S { f } = s;
    f
}
