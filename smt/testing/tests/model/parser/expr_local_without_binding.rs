use rusmart_smt_remark_derive::smt_impl;
use rusmart_smt_stdlib::Boolean;

#[smt_impl]
fn foo() -> Boolean {
    let x;
    x = Boolean::from(true);
    x
}
