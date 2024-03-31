use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::dt::{Boolean, Integer, Rational, Text};

#[smt_impl]
fn bar() -> (Boolean, Integer, Rational, Text) {
    (
        Boolean::from(false),
        Integer::from(0),
        Rational::from(0),
        Text::from(""),
    )
}
