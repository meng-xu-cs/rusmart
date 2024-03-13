use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::SMT;

#[smt_type]
struct S<T1: SMT, T2: SMT> {
    t1: T1,
    t2: T2,
}

#[smt_impl(method = m1)]
fn f1<T1: SMT, T2: SMT>(s: S<T1, T2>) -> S<T1, T2> {
    s
}
