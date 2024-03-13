use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Map, Seq, Set, SMT};

#[smt_type]
struct S<T1: SMT, T2: SMT> {
    t1: T1,
    t2: T2,
}

#[smt_impl(method = m1)]
fn f1<T1: SMT, T2: SMT>(s: S<T1, T2>) -> S<T1, T2> {
    s
}

#[smt_impl(method = m2)]
fn f2<T1: SMT, T2: SMT>(s: S<Set<T1>, Seq<T2>>) -> S<Set<T1>, Seq<T2>> {
    s
}

#[smt_impl(method = m3)]
fn f3<T: SMT>(s: S<Set<T>, Map<T, T>>) -> S<Set<T>, Map<T, T>> {
    s
}
