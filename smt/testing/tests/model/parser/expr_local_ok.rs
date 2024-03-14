use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::dt::{Boolean, Integer};

#[smt_impl]
fn f1() -> Boolean {
    let x = Boolean::from(false);
    x
}

#[smt_impl]
fn f2() -> (Boolean, Integer) {
    let x = (Boolean::from(false), Integer::from(0));
    x
}

#[smt_impl]
fn f3() -> (Boolean, Integer) {
    let (x, y) = (Boolean::from(false), Integer::from(0));
    (x, y)
}

#[smt_impl]
fn f4(a: Boolean) -> Integer {
    let x = if *a {
        Integer::from(0)
    } else {
        Integer::from(1)
    };
    x
}
