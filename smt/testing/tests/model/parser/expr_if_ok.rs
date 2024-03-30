use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::dt::Boolean;

#[smt_impl]
fn f1(x: Boolean) -> Boolean {
    if *x {
        Boolean::from(true)
    } else {
        Boolean::from(false)
    }
}
