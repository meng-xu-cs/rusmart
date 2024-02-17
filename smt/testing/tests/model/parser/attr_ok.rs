use rusmart_smt_remark::{smt_axiom, smt_impl, smt_spec, smt_type};
use rusmart_smt_stdlib::dt::Boolean;

/* annotations for types */

#[smt_type]
enum E {
    V0,
}

#[smt_type]
struct S {
    f1: Boolean,
}

/* annotations for impls and specs */

#[smt_impl]
fn f1() -> Boolean {
    Boolean::from(false)
}

#[smt_spec]
fn f2() -> Boolean {
    Boolean::from(false)
}

#[smt_impl(method = m3)]
fn f3(s: S) -> Boolean {
    s.f1.not()
}

#[smt_spec(method = m4)]
fn f4(s: S) -> Boolean {
    s.f1.not()
}

#[smt_impl(specs = f6)]
fn f5(s: S) -> Boolean {
    f3(s)
}

#[smt_spec(impls = f5)]
fn f6(s: S) -> Boolean {
    f4(s)
}

/* annotations for axioms */

#[smt_axiom]
fn a1() -> Boolean {
    Boolean::from(false)
}
