use rusmart_smt_remark::smt_type;
use rusmart_smt_stdlib::dt::SMT;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
struct NotSMT {}

#[smt_type]
struct S {
    f: NotSMT,
}
