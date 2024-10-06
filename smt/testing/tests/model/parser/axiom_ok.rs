use rusmart_smt_remark_derive::{smt_axiom, smt_spec};
use rusmart_smt_stdlib::{Boolean, SMT};

#[smt_spec]
fn force_eq<T: SMT>(a: T, b: T) -> Boolean {
    unimplemented!()
}

#[smt_axiom]
fn axiom_force_eq<T: SMT>(a: T, b: T) -> Boolean {
    force_eq(a, b)
}
