use rusmart_smt_remark_derive::smt_type;
use rusmart_smt_stdlib::SMT;

#[smt_type]
struct S<T: SMT> {
    f: T,
}

#[smt_type]
struct R<P: SMT, Q: SMT> {
    f1: P,
    f2: S<Q>,
}
