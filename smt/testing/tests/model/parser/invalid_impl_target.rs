use rusmart_smt_remark::smt_spec;
use rusmart_smt_stdlib::dt::Boolean;

#[smt_spec(impls = does_non_exist)]
fn foo() -> Boolean {
    Boolean::from(false)
}
