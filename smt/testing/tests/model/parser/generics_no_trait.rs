use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::dt::SMT;

#[smt_impl]
fn foo<T>(t: T) -> T {
    t
}
