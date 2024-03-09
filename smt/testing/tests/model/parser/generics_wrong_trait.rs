use rusmart_smt_remark::smt_impl;

trait NotSMT {}

#[smt_impl]
fn foo<T: NotSMT>(t: T) -> T {
    t
}
