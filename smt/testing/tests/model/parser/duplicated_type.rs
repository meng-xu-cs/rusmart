mod a {
    use rusmart_smt_remark_derive::smt_type;
    use rusmart_smt_stdlib::{Boolean, SMT};

    #[smt_type]
    struct S {
        f: Boolean,
    }
}

mod b {
    use rusmart_smt_remark_derive::smt_type;
    use rusmart_smt_stdlib::{Boolean, SMT};

    #[smt_type]
    struct S {
        f: Boolean,
    }
}
