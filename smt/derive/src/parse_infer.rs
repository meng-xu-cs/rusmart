use std::collections::{BTreeMap, BTreeSet};

use crate::parse_path::{FuncName, TypeName};

/// A type variable representing either a concrete type or a symbolic (i.e., to be inferred) one
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
enum TypeVar {
    /// to be inferred
    Unknown(usize),
    /// boolean
    Boolean,
    /// integer (unlimited precision)
    Integer,
    /// rational numbers (unlimited precision)
    Rational,
    /// string
    Text,
    /// inductively defined type
    Cloak(Box<TypeVar>),
    /// SMT-sequence
    Seq(Box<TypeVar>),
    /// SMT-set
    Set(Box<TypeVar>),
    /// SMT-array
    Map(Box<TypeVar>, Box<TypeVar>),
    /// dynamic error type
    Error,
    /// user-defined type
    User(TypeName),
}

/// A function type, with inference allowed
#[derive(Ord, PartialOrd, Eq, PartialEq)]
struct TypeFn {
    qualifier: Option<String>,
    params: Vec<TypeVar>,
    ret_ty: TypeVar,
}

/// A database of intrinsic and declared function with their typing information
pub struct FuncTypeDatabase {
    db: BTreeMap<FuncName, BTreeSet<TypeFn>>,
}

impl FuncTypeDatabase {
    /// Create an empty database
    fn new() -> Self {
        Self {
            db: BTreeMap::new(),
        }
    }

    /// Register an intrinsic
    fn register_intrinsic(
        &mut self,
        ident: &str,
        qualifier: &str,
        params: Vec<TypeVar>,
        ret_ty: TypeVar,
    ) {
        let name = FuncName::for_intrinsic(ident);
        let func = TypeFn {
            qualifier: Some(qualifier.to_string()),
            params,
            ret_ty,
        };
        let inserted = self.db.entry(name).or_default().insert(func);
        if !inserted {
            panic!(
                "duplicated registration of intrinsics: {}::{}",
                qualifier, ident
            );
        }
    }

    /// Pre-populate the database with intrinsics
    pub fn with_intrinsics() -> Self {
        use TypeVar::*;

        let mut db = Self::new();

        // logical operators
        db.register_intrinsic("not", "Boolean", vec![Boolean], Boolean);
        db.register_intrinsic("and", "Boolean", vec![Boolean, Boolean], Boolean);
        db.register_intrinsic("or", "Boolean", vec![Boolean, Boolean], Boolean);
        db.register_intrinsic("xor", "Boolean", vec![Boolean, Boolean], Boolean);

        // arithmetic
        db.register_intrinsic("add", "Integer", vec![Integer, Integer], Integer);
        db.register_intrinsic("add", "Rational", vec![Rational, Rational], Rational);

        db.register_intrinsic("sub", "Integer", vec![Integer, Integer], Integer);
        db.register_intrinsic("sub", "Rational", vec![Rational, Rational], Rational);

        db.register_intrinsic("mul", "Integer", vec![Integer, Integer], Integer);
        db.register_intrinsic("mul", "Rational", vec![Rational, Rational], Rational);

        db.register_intrinsic("div", "Integer", vec![Integer, Integer], Integer);
        db.register_intrinsic("div", "Rational", vec![Rational, Rational], Rational);

        db.register_intrinsic("rem", "Integer", vec![Integer, Integer], Integer);

        // (in)equality operators
        db.register_intrinsic("eq", "Boolean", vec![Boolean, Boolean], Boolean);
        db.register_intrinsic("eq", "Integer", vec![Integer, Integer], Boolean);
        db.register_intrinsic("eq", "Rational", vec![Rational, Rational], Boolean);
        db.register_intrinsic("eq", "Text", vec![Text, Text], Boolean);
        db.register_intrinsic(
            "eq",
            "Cloak",
            vec![Cloak(Unknown(0).into()), Cloak(Unknown(0).into())],
            Boolean,
        );
        db.register_intrinsic(
            "eq",
            "Seq",
            vec![Seq(Unknown(0).into()), Seq(Unknown(0).into())],
            Boolean,
        );
        db.register_intrinsic(
            "eq",
            "Set",
            vec![Set(Unknown(0).into()), Set(Unknown(0).into())],
            Boolean,
        );
        db.register_intrinsic(
            "eq",
            "Map",
            vec![
                Map(Unknown(0).into(), Unknown(1).into()),
                Map(Unknown(0).into(), Unknown(1).into()),
            ],
            Boolean,
        );
        db.register_intrinsic("eq", "Error", vec![Error, Error], Boolean);

        db.register_intrinsic("ne", "Boolean", vec![Boolean, Boolean], Boolean);
        db.register_intrinsic("ne", "Integer", vec![Integer, Integer], Boolean);
        db.register_intrinsic("ne", "Rational", vec![Rational, Rational], Boolean);
        db.register_intrinsic("ne", "Text", vec![Text, Text], Boolean);
        db.register_intrinsic(
            "ne",
            "Cloak",
            vec![Cloak(Unknown(0).into()), Cloak(Unknown(0).into())],
            Boolean,
        );
        db.register_intrinsic(
            "ne",
            "Seq",
            vec![Seq(Unknown(0).into()), Seq(Unknown(0).into())],
            Boolean,
        );
        db.register_intrinsic(
            "ne",
            "Set",
            vec![Set(Unknown(0).into()), Set(Unknown(0).into())],
            Boolean,
        );
        db.register_intrinsic(
            "ne",
            "Map",
            vec![
                Map(Unknown(0).into(), Unknown(1).into()),
                Map(Unknown(0).into(), Unknown(1).into()),
            ],
            Boolean,
        );
        db.register_intrinsic("ne", "Error", vec![Error, Error], Boolean);

        // comparison operators
        db.register_intrinsic("lt", "Integer", vec![Integer, Integer], Boolean);
        db.register_intrinsic("lt", "Rational", vec![Rational, Rational], Rational);
        db.register_intrinsic("lt", "Text", vec![Text, Text], Rational);

        db.register_intrinsic("le", "Integer", vec![Integer, Integer], Boolean);
        db.register_intrinsic("le", "Rational", vec![Rational, Rational], Rational);
        db.register_intrinsic("le", "Text", vec![Text, Text], Rational);

        db.register_intrinsic("ge", "Integer", vec![Integer, Integer], Boolean);
        db.register_intrinsic("ge", "Rational", vec![Rational, Rational], Rational);

        db.register_intrinsic("gt", "Integer", vec![Integer, Integer], Boolean);
        db.register_intrinsic("gt", "Rational", vec![Rational, Rational], Rational);

        // cloaking
        db.register_intrinsic(
            "shield",
            "Cloak",
            vec![Unknown(0)],
            Cloak(Unknown(0).into()),
        );

        db.register_intrinsic(
            "reveal",
            "Cloak",
            vec![Cloak(Unknown(0).into())],
            Unknown(0),
        );

        // collections
        db.register_intrinsic("empty", "Seq", vec![], Seq(Unknown(0).into()));
        db.register_intrinsic("empty", "Set", vec![], Set(Unknown(0).into()));
        db.register_intrinsic(
            "empty",
            "Map",
            vec![],
            Map(Unknown(0).into(), Unknown(1).into()),
        );

        db.register_intrinsic("length", "Seq", vec![Seq(Unknown(0).into())], Integer);
        db.register_intrinsic("length", "Set", vec![Set(Unknown(0).into())], Integer);
        db.register_intrinsic(
            "length",
            "Map",
            vec![Map(Unknown(0).into(), Unknown(1).into())],
            Integer,
        );

        // seq
        db.register_intrinsic(
            "append",
            "Seq",
            vec![Seq(Unknown(0).into()), Unknown(0)],
            Seq(Unknown(0).into()),
        );
        db.register_intrinsic(
            "at_unchecked",
            "Seq",
            vec![Seq(Unknown(0).into()), Integer],
            Unknown(0),
        );
        db.register_intrinsic(
            "includes",
            "Seq",
            vec![Seq(Unknown(0).into()), Unknown(0)],
            Boolean,
        );

        // set
        db.register_intrinsic(
            "insert",
            "Set",
            vec![Set(Unknown(0).into()), Unknown(0)],
            Set(Unknown(0).into()),
        );
        db.register_intrinsic(
            "contains",
            "Set",
            vec![Set(Unknown(0).into()), Unknown(0)],
            Boolean,
        );

        // map
        db.register_intrinsic(
            "put_unchecked",
            "Map",
            vec![
                Map(Unknown(0).into(), Unknown(1).into()),
                Unknown(0),
                Unknown(1),
            ],
            Map(Unknown(0).into(), Unknown(1).into()),
        );
        db.register_intrinsic(
            "get_unchecked",
            "Map",
            vec![Map(Unknown(0).into(), Unknown(1).into()), Unknown(0)],
            Unknown(1),
        );
        db.register_intrinsic(
            "contains_key",
            "Map",
            vec![Map(Unknown(0).into(), Unknown(1).into()), Unknown(0)],
            Boolean,
        );

        // error
        db.register_intrinsic("fresh", "Error", vec![], Error);

        db.register_intrinsic("merge", "Error", vec![Error, Error], Error);

        // done
        db
    }
}
