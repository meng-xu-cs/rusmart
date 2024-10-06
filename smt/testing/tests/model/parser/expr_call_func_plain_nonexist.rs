use rusmart_smt_remark_derive::smt_impl;
use rusmart_smt_stdlib::Boolean;

fn foo() -> Boolean {
    Boolean::from(false)
}

#[smt_impl]
fn bar() -> Boolean {
    foo()
}
