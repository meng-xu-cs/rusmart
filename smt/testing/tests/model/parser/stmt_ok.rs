use rusmart_smt_remark::smt_impl;
use rusmart_smt_stdlib::Boolean;

#[smt_impl]
fn f1() -> Boolean {
    let x = Boolean::from(false);
    x
}

#[smt_impl]
fn f2() -> Boolean {
    let x = { Boolean::from(false) };
    x
}

#[smt_impl]
fn f3() -> Boolean {
    let x = {
        let a = Boolean::from(false);
        a
    };
    x
}

#[smt_impl]
fn f4() -> Boolean {
    let x = {
        let a = Boolean::from(false);
        let b = Boolean::from(true);
        a.implies(b)
    };
    x
}

#[smt_impl]
fn f5() -> Boolean {
    let x = {
        let x = Boolean::from(true);
        x
    };
    x
}
