use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, Integer, SMT};

#[smt_type]
struct S1 {
    f1: Boolean,
    f2: Integer,
}

#[smt_impl]
fn f1(s: S1) -> Boolean {
    s.f1
}

#[smt_type]
struct S2<T1: SMT, T2: SMT> {
    f1: T1,
    f2: T2,
}

#[smt_impl]
fn f2<T1: SMT, T2: SMT>(s: S2<T1, T2>) -> (T1, T2) {
    (s.f1, s.f2)
}

#[smt_type]
struct S3(Integer, Boolean);

#[smt_impl]
fn f3(s: S3) -> Integer {
    s.0
}

#[smt_type]
struct S4<T1: SMT, T2: SMT>(T1, T2);

#[smt_impl]
fn f4<T1: SMT, T2: SMT>(s: S4<T1, T2>) -> (T2, T1) {
    (s.1, s.0)
}
