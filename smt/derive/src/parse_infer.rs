use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};

use crate::parse_func::FuncSig;
use crate::parse_path::{FuncName, TypeName};
use crate::parse_type::TypeTag;

/// A reference to either a concrete type or a symbolic (i.e., to be inferred) type
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
enum TypeRef {
    /// to be inferred, param id is relative to an enclosing context
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
    Cloak(Box<TypeRef>),
    /// SMT-sequence
    Seq(Box<TypeRef>),
    /// SMT-set
    Set(Box<TypeRef>),
    /// SMT-array
    Map(Box<TypeRef>, Box<TypeRef>),
    /// dynamic error type
    Error,
    /// user-defined type
    User(TypeName),
}

impl From<&TypeTag> for TypeRef {
    fn from(ty: &TypeTag) -> Self {
        match ty {
            TypeTag::Boolean => Self::Boolean,
            TypeTag::Integer => Self::Integer,
            TypeTag::Rational => Self::Rational,
            TypeTag::Text => Self::Text,
            TypeTag::Cloak(sub) => Self::Cloak(Box::new(sub.as_ref().into())),
            TypeTag::Seq(sub) => Self::Seq(Box::new(sub.as_ref().into())),
            TypeTag::Set(sub) => Self::Set(Box::new(sub.as_ref().into())),
            TypeTag::Map(key, val) => {
                Self::Map(Box::new(key.as_ref().into()), Box::new(val.as_ref().into()))
            }
            TypeTag::Error => Self::Error,
            TypeTag::User(name) => Self::User(name.clone()),
        }
    }
}

/// A function type, with inference allowed
#[derive(Ord, PartialOrd, Eq, PartialEq)]
struct TypeFn {
    qualifier: Option<String>,
    params: Vec<TypeRef>,
    ret_ty: TypeRef,
}

/// A database for type inference
pub struct InferDatabase {
    /// intrinsic and declared function with their typing information
    methods: BTreeMap<FuncName, BTreeSet<TypeFn>>,
}

impl InferDatabase {
    /// Create an empty database
    fn new() -> Self {
        Self {
            methods: BTreeMap::new(),
        }
    }

    /// Register an intrinsic
    fn register_intrinsic(
        &mut self,
        ident: &str,
        qualifier: &str,
        params: Vec<TypeRef>,
        ret_ty: TypeRef,
    ) {
        let name = FuncName::for_intrinsic(ident);
        let func = TypeFn {
            qualifier: Some(qualifier.to_string()),
            params,
            ret_ty,
        };
        let inserted = self.methods.entry(name).or_default().insert(func);
        if !inserted {
            panic!(
                "duplicated registration of intrinsics: {}::{}",
                qualifier, ident
            );
        }
    }

    /// Pre-populate the database with intrinsics
    pub fn with_intrinsics() -> Self {
        use TypeRef::*;

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

    /// Register a user-defined SMT type
    pub fn register_user_type(&mut self, name: &TypeName) {
        let func_eq = TypeFn {
            qualifier: Some(name.to_string()),
            params: vec![TypeRef::User(name.clone()), TypeRef::User(name.clone())],
            ret_ty: TypeRef::Boolean,
        };
        let inserted = self
            .methods
            .entry(FuncName::for_intrinsic("eq"))
            .or_default()
            .insert(func_eq);
        if !inserted {
            panic!("duplicated registration of smt type: {}::eq", name);
        }

        let func_ne = TypeFn {
            qualifier: Some(name.to_string()),
            params: vec![TypeRef::User(name.clone()), TypeRef::User(name.clone())],
            ret_ty: TypeRef::Boolean,
        };
        let inserted = self
            .methods
            .entry(FuncName::for_intrinsic("ne"))
            .or_default()
            .insert(func_ne);
        if !inserted {
            panic!("duplicated registration of smt type: {}::ne", name);
        }
    }

    /// Register a user-defined function (spec or impl)
    pub fn register_user_func(&mut self, name: &FuncName, sig: &FuncSig) {
        let params = sig.param_vec().iter().map(|(_, ty)| ty.into()).collect();
        let ret_ty = sig.ret_ty().into();

        // if the first parameter is a user-defined type, mark it as a qualifier
        let qualifier = match sig.param_vec().first() {
            Some((_, TypeTag::User(tn))) => Some(tn.to_string()),
            _ => None,
        };

        let func = TypeFn {
            qualifier,
            params,
            ret_ty,
        };
        let inserted = self.methods.entry(name.clone()).or_default().insert(func);
        if !inserted {
            panic!("duplicated registration of smt func: {}", name);
        }
    }

    /// Report number of entries in the database
    pub fn size(&self) -> usize {
        self.methods.len()
    }
}

/// Context manager for type inference
pub struct TypeUnifier {
    /// holds the set of possible candidates associated with each type parameter
    params: RefCell<BTreeMap<usize, Option<BTreeSet<TypeRef>>>>,
}

impl TypeUnifier {
    /// create with an empty type unification context
    pub fn new() -> Self {
        Self {
            params: BTreeMap::new().into(),
        }
    }

    /// Create a new type parameter
    pub fn mk_param(&self) -> TypeVar {
        let mut params = self.params.borrow_mut();

        let id = params.len();
        let existing = params.insert(id, None);
        assert!(existing.is_none());

        TypeVar::Param(TypeParam { id, unifier: self })
    }
}

/// Represents a type parameter
#[derive(Clone)]
pub struct TypeParam<'ty> {
    id: usize,
    unifier: &'ty TypeUnifier,
}

/// A type variable representing either a concrete type or a symbolic (i.e., to be inferred) one
#[derive(Clone)]
pub enum TypeVar<'ty> {
    /// to be inferred
    Param(TypeParam<'ty>),
    /// boolean
    Boolean,
    /// integer (unlimited precision)
    Integer,
    /// rational numbers (unlimited precision)
    Rational,
    /// string
    Text,
    /// inductively defined type
    Cloak(Box<TypeVar<'ty>>),
    /// SMT-sequence
    Seq(Box<TypeVar<'ty>>),
    /// SMT-set
    Set(Box<TypeVar<'ty>>),
    /// SMT-array
    Map(Box<TypeVar<'ty>>, Box<TypeVar<'ty>>),
    /// dynamic error type
    Error,
    /// user-defined type
    User(TypeName),
}

impl From<&TypeTag> for TypeVar<'static> {
    fn from(ty: &TypeTag) -> Self {
        match ty {
            TypeTag::Boolean => Self::Boolean,
            TypeTag::Integer => Self::Integer,
            TypeTag::Rational => Self::Rational,
            TypeTag::Text => Self::Text,
            TypeTag::Cloak(sub) => Self::Cloak(Box::new(sub.as_ref().into())),
            TypeTag::Seq(sub) => Self::Seq(Box::new(sub.as_ref().into())),
            TypeTag::Set(sub) => Self::Set(Box::new(sub.as_ref().into())),
            TypeTag::Map(key, val) => {
                Self::Map(Box::new(key.as_ref().into()), Box::new(val.as_ref().into()))
            }
            TypeTag::Error => Self::Error,
            TypeTag::User(name) => Self::User(name.clone()),
        }
    }
}
