use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::dt::SMT;

#[smt_impl]
fn foo(x: impl SMT) -> impl SMT {
    x
}
