use rusmart_smt_remark_derive::{smt_impl, smt_type};
use rusmart_smt_stdlib::{Boolean, SMT};

#[smt_type]
enum E1 {
    V1 { f: Boolean },
}

#[smt_type]
enum E2<K: SMT, V: SMT> {
    V2 { k: K, v: V },
}

#[smt_impl]
fn f1() -> E1 {
    E1::V1 {
        f: Boolean::from(false),
    }
}

#[smt_impl]
fn f2<K: SMT, V: SMT>(k: K, v: V) -> E2<K, V> {
    E2::V2 { k, v }
}

#[smt_impl]
fn f3<T: SMT>(t: T) -> E2<T, T> {
    E2::V2 { k: t, v: t }
}

#[smt_impl]
fn f4<T: SMT>(t: T) -> E2<T, Boolean> {
    E2::<T, Boolean>::V2 {
        k: t,
        v: Boolean::from(false),
    }
}
