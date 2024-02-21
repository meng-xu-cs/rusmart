use rusmart_smt_remark::smt_type;

trait NotSMT {}

#[smt_type]
struct S<T: NotSMT> {
    f: T,
}
