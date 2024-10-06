use rusmart_smt_remark_derive::smt_impl;
use rusmart_smt_stdlib::Boolean;

#[smt_impl]
fn f1(x: Boolean) -> Boolean {
    if *x {
        Boolean::from(true)
    } else {
        Boolean::from(false)
    }
}

#[smt_impl]
fn f2(x: Boolean, y: Boolean) -> Boolean {
    if *x {
        Boolean::from(true)
    } else if *y {
        Boolean::from(true)
    } else {
        Boolean::from(false)
    }
}
