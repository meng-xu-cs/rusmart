use rusmart_smt_remark::smt_impl;

struct NotSMT {}

#[smt_impl]
fn foo(t: NotSMT) -> NotSMT {
    t
}
