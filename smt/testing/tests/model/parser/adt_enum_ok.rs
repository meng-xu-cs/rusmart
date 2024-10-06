use rusmart_smt_remark_derive::smt_type;
use rusmart_smt_stdlib::{Boolean, Error, Integer, Rational, Text, SMT};

#[smt_type]
enum E1 {
    V1,
    V2(Boolean),
    V3 { f1: Integer },
    V4((Boolean, Rational)),
    V5 { f1: (Text, Boolean) },
    V6(Error),
}
