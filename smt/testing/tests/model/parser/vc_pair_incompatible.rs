use rusmart_smt_remark::{smt_impl, smt_spec};
use rusmart_smt_stdlib::dt::{Boolean, Integer, Rational};

#[smt_impl(specs = spec_foo)]
fn impl_foo(a: Integer, b: Integer) -> Boolean {
    a.eq(b)
}

#[smt_spec(impls = impl_foo)]
fn spec_foo(a: Rational, b: Rational) -> Boolean {
    a.eq(b)
}
