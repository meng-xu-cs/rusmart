use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, SMT};

#[smt_type]
struct S1 {
    f: Boolean,
}

#[smt_type]
struct S2<K: SMT, V: SMT> {
    k: K,
    v: V,
}

#[smt_impl]
fn f1() -> S1 {
    S1 {
        f: Boolean::from(false),
    }
}

#[smt_impl]
fn f2<T: SMT>(t: T) -> S2<T, T> {
    S2 { k: t, v: t }
}
