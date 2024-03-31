use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, SMT};

#[smt_type]
struct S(Boolean);

#[smt_impl]
fn f() -> S {
    S::default()
}
