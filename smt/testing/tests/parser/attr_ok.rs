use rusmart_smt_remark::smt_type;
use rusmart_smt_stdlib::dt::Boolean;

#[smt_type]
enum E {
    V0,
}

#[smt_type]
struct S {
    f1: Boolean,
}
