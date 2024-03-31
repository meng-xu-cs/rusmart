use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::dt::{Boolean, Integer, SMT};

#[smt_impl]
fn f1() -> (Boolean, Integer) {
    (Boolean::from(false), Integer::from(0))
}

#[smt_impl]
fn f2<K: SMT, V: SMT>(k: K, v: V) -> (K, V) {
    (k, v)
}

#[smt_impl]
fn f3<T: SMT>(t: T) -> (T, T) {
    (t, t)
}
