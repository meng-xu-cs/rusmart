use rusmart_smt_remark_derive::{smt_impl, smt_type};
use rusmart_smt_stdlib::{Boolean, Integer, Rational, Text, SMT};

#[smt_impl]
fn int_eq(a: Integer, b: Integer) -> Boolean {
    a.eq(b)
}

#[smt_impl]
fn str_ne(a: Text, b: Text) -> Boolean {
    a.ne(b)
}

#[smt_type]
struct S(Rational);

#[smt_impl]
fn usr_s_eq(a: S, b: S) -> Boolean {
    a.eq(b)
}

#[smt_type]
enum E {
    None,
    Some(Integer),
}

#[smt_impl]
fn usr_e_ne(a: E, b: E) -> Boolean {
    a.ne(b)
}

#[smt_impl]
fn param_t_eq<T: SMT>(a: T, b: T) -> Boolean {
    a.ne(b)
}

#[smt_type]
struct G<T: SMT> {
    f: T,
}

#[smt_impl]
fn param_t_ne<T: SMT>(a: G<T>, b: G<T>) -> Boolean {
    a.ne(b)
}
