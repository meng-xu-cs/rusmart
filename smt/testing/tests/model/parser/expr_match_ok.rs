use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, SMT};

#[smt_type]
enum E1 {
    None,
    Some(Boolean),
}

#[smt_impl]
fn f1(e: E1) -> Boolean {
    match e {
        E1::None => Boolean::from(false),
        E1::Some(v) => v,
    }
}

#[smt_impl]
fn f2(e: E1) -> Boolean {
    match (e) {
        E1::None => Boolean::from(false),
        E1::Some(v) => v,
    }
}

#[smt_impl]
fn f3(a: E1, b: E1) -> Boolean {
    match (a, b) {
        (E1::None, E1::None) => Boolean::from(false),
        (E1::None, _) => Boolean::from(false),
        (_, E1::None) => Boolean::from(false),
        (E1::Some(v1), E1::Some(v2)) => v1.xor(v2),
    }
}

#[smt_impl]
fn f4(a: E1, b: E1) -> Boolean {
    match (a, b) {
        (E1::Some(v1), E1::Some(v2)) => v1.xor(v2),
        (_, _) => Boolean::from(false),
    }
}

#[smt_impl]
fn f5(e: E1) -> Boolean {
    match e {
        _ => Boolean::from(false),
    }
}

#[smt_impl]
fn f6(e: E1) -> Boolean {
    match e {
        E1::None => Boolean::from(false),
        E1::Some(_) => Boolean::from(true),
    }
}

#[smt_type]
enum E2<T: SMT, K: SMT, V: SMT> {
    Zero,
    One(T),
    Two { k: K, v: V },
}

#[smt_impl]
fn f7<T: SMT, K: SMT, V: SMT>(e: E2<T, K, V>, a: T, b: K, c: V) -> (T, K, V) {
    match e {
        E2::Zero => (a, b, c),
        E2::One(t) => (t, b, c),
        E2::Two { k, v } => (a, k, v),
    }
}
