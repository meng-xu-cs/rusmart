mod a {
    use rusmart_smt_remark_derive::smt_axiom;
    use rusmart_smt_stdlib::Boolean;

    #[smt_axiom]
    fn foo() -> Boolean {
        Boolean::from(false)
    }
}

mod b {
    use rusmart_smt_remark_derive::smt_axiom;
    use rusmart_smt_stdlib::Boolean;

    #[smt_axiom]
    fn foo() -> Boolean {
        Boolean::from(false)
    }
}
