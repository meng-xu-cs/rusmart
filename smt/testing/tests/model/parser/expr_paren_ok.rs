use rusmart_smt_remark_derive::smt_impl;
use rusmart_smt_stdlib::{Boolean, Integer, SMT};

#[smt_impl]
fn f1(s: Boolean) -> Boolean {
    (s)
}

#[smt_impl]
fn f2<T: SMT>(s: T) -> T {
    (s)
}

#[smt_impl]
fn f3<K: SMT, V: SMT>(k: K, v: V) -> (K, V) {
    ((k, v))
}

#[smt_impl]
fn f4(a: Integer, b: Integer) -> Integer {
    ((a.add(b)).sub(Integer::from(0)))
}
