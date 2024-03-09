use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::dt::SMT;

trait NotSMT {}

#[smt_impl]
fn foo<T: SMT + NotSMT>(t: T) -> T {
    t
}
