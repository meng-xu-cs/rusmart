use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, SMT};

#[smt_type]
enum E1 {
    V0,
    V1(Boolean),
    V2 { f: Boolean },
}

#[smt_type]
enum E2<T: SMT> {
    V0,
    V1(T),
    V2 { f: T },
}

#[smt_impl]
fn f1() -> E1 {
    E1::V0
}

#[smt_impl]
fn f2<T: SMT>() -> E2<T> {
    E2::V0
}

#[smt_impl]
fn f3<T: SMT>() -> E2<T> {
    E2::<T>::V0
}
