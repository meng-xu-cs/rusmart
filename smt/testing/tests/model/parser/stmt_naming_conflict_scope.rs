use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::Boolean;

#[smt_impl]
fn f() -> Boolean {
    let x = Boolean::from(false);
    {
        let x = x.not();
        x
    }
}
