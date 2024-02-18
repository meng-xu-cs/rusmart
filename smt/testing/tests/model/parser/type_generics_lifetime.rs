use rusmart_smt_remark::smt_type;
use rusmart_smt_stdlib::dt::Boolean;

#[smt_type]
struct S<'a> {
    f: &'a Boolean,
}
