use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::Boolean;

#[smt_impl]
fn f(x: Boolean) -> Boolean {
    let x = Boolean::from(false);
    x
}
