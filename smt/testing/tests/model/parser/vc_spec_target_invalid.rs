use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::Boolean;

#[smt_impl(specs = does_non_exist)]
fn foo() -> Boolean {
    Boolean::from(false)
}
