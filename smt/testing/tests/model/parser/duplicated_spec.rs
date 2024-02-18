mod a {
    use rusmart_smt_remark::smt_spec;
    use rusmart_smt_stdlib::dt::Boolean;

    #[smt_spec]
    fn foo() -> Boolean {
        Boolean::from(false)
    }
}

mod b {
    use rusmart_smt_remark::smt_spec;
    use rusmart_smt_stdlib::dt::Boolean;

    #[smt_spec]
    fn foo() -> Boolean {
        Boolean::from(false)
    }
}
