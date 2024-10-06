use rusmart_smt_remark_derive::smt_impl;
use rusmart_smt_stdlib::SMT;

#[smt_impl]
fn f<T: SMT>() -> T {
    T::default()
}
