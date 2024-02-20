use rusmart_smt_remark::smt_type;
use rusmart_smt_stdlib::dt::Boolean;

#[smt_type]
struct S1 {
    f: Boolean,
}

#[smt_type]
enum E1 {
    V1,
    V2(S1),
    V3 { f1: S1 },
}
