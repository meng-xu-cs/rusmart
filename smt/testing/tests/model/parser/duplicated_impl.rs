mod a {
    use rusmart_smt_remark::smt_impl;
    use rusmart_smt_stdlib::dt::Boolean;

    #[smt_impl]
    fn foo() -> Boolean {
        Boolean::from(false)
    }
}

mod b {
    use rusmart_smt_remark::smt_impl;
    use rusmart_smt_stdlib::dt::Boolean;

    #[smt_impl]
    fn foo() -> Boolean {
        Boolean::from(false)
    }
}
