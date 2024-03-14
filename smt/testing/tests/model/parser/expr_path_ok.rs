use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, SMT};

#[smt_type]
enum E {
    V0,
    V1(Boolean),
    V2 { f: Boolean },
}

#[smt_impl]
fn f1() -> E {
    E::V0
}
