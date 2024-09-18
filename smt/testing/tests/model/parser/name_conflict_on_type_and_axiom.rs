mod a {
    use rusmart_smt_remark::smt_type;
    use rusmart_smt_stdlib::{Boolean, SMT};

    #[smt_type]
    struct foo {
        f: Boolean,
    }
}

mod b {
    use rusmart_smt_remark::smt_axiom;
    use rusmart_smt_stdlib::Boolean;

    #[smt_axiom]
    fn foo() -> Boolean {
        Boolean::from(false)
    }
}
