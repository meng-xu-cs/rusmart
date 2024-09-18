use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::Boolean;

#[smt_impl]
fn f() -> Boolean {
    if true {
        Boolean::from(true)
    } else {
        Boolean::from(false)
    }
}
