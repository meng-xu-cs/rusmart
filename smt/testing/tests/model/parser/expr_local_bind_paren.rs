use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::dt::Boolean;

#[smt_impl]
fn foo() -> Boolean {
    let (x) = Boolean::from(false);
    x
}
