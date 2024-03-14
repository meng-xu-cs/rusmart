use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::SMT;

#[smt_type]
enum E {
    V0,
}

#[smt_impl]
fn f1() -> E {
    crate::model::parser::expr_path_with_namespace::E::V0
}
