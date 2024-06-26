use rusmart_smt_remark::smt_type;
use rusmart_smt_stdlib::dt::{Boolean, Integer, SMT};

#[smt_type]
struct S<T1: SMT, T2: SMT>(Boolean, Integer, T1, T2, (T1, T2));
