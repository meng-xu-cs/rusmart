use rusmart_smt_remark_derive::smt_spec;
use rusmart_smt_stdlib::Boolean;

#[smt_spec(impls = does_non_exist)]
fn foo() -> Boolean {
    Boolean::from(false)
}
