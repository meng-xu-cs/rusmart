mod a {
    use rusmart_smt_remark::smt_type;
    use rusmart_smt_stdlib::dt::{Boolean, SMT};

    #[smt_type]
    struct S {
        f: Boolean,
    }
}

mod b {
    use rusmart_smt_remark::smt_type;
    use rusmart_smt_stdlib::dt::{Boolean, SMT};

    #[smt_type]
    struct S {
        f: Boolean,
    }
}
