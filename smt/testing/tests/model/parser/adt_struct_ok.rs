use rusmart_smt_remark::smt_type;
use rusmart_smt_stdlib::dt::{Boolean, Cloak, Map, Seq, Set, SMT};

#[smt_type]
struct S1 {
    f1: Boolean,
    f2: (Boolean, Boolean),
    f3: Seq<(Boolean, Boolean)>,
}

#[smt_type]
struct S2 {
    f1: S1,
    f2: Cloak<S1>,
    f3: Seq<S1>,
    f4: Set<S1>,
    f5: Map<S1, S1>,
    f6: Map<(S1, S1), (S1, S1, S1)>,
}

#[smt_type]
struct S3<T1: SMT, T2: SMT, T3: SMT> {
    f1: S1,
    f2: Cloak<T1>,
    f3: Seq<T2>,
    f4: Set<T3>,
    f5: Map<T2, T3>,
    f6: Map<(T1, T3), (T1, T2, T3)>,
}
