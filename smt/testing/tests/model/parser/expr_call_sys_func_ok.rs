use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, Integer, Rational, Text, SMT};

#[smt_impl]
fn int_eq(a: Integer, b: Integer) -> Boolean {
    Integer::eq(a, b)
}

#[smt_impl]
fn str_ne(a: Text, b: Text) -> Boolean {
    Text::ne(a, b)
}

#[smt_type]
struct S(Rational);

#[smt_impl]
fn usr_s_eq(a: S, b: S) -> Boolean {
    S::eq(a, b)
}

#[smt_type]
enum E {
    None,
    Some(Integer),
}

#[smt_impl]
fn usr_e_ne(a: E, b: E) -> Boolean {
    E::ne(a, b)
}

#[smt_impl]
fn param_t_eq<T: SMT>(a: T, b: T) -> Boolean {
    T::eq(a, b)
}

#[smt_type]
struct G<T: SMT> {
    f: T,
}

#[smt_impl]
fn param_t_ne<T: SMT>(a: G<T>, b: G<T>) -> Boolean {
    a.ne(b)
}
