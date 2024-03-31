use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::dt::Boolean;

fn foo() -> Boolean {
    Boolean::from(false)
}

#[smt_impl]
fn bar() -> Boolean {
    foo()
}
