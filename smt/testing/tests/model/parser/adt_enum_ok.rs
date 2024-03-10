use rusmart_smt_remark::smt_type;
use rusmart_smt_stdlib::dt::{Boolean, SMT};

#[smt_type]
enum E1 {
    V1,
    V2(Boolean),
    V3 { f1: Boolean },
    V4((Boolean, Boolean)),
    V5 { f1: (Boolean, Boolean) },
}
