use rusmart_smt_remark::smt_type;
use rusmart_smt_stdlib::dt::SMT;

trait NotSMT {}

#[smt_type]
struct S<T: SMT + NotSMT> {
    f: T,
}
