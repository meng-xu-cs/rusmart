use rusmart_smt_remark::smt_impl;

#[smt_impl]
fn foo(x: fn()) -> fn() {
    x
}
