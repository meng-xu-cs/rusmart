use rusmart_smt_remark::smt_type;
use rusmart_smt_stdlib::dt::SMT;

#[smt_type]
struct S<T: SMT> {
    f: T,
}
