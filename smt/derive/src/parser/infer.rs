use std::collections::{BTreeMap, BTreeSet};

use crate::parser::func::{FuncName, FuncSig};
use crate::parser::generics::Generics;
use crate::parser::name::{TypeParamName, UsrFuncName, UsrTypeName};
use crate::parser::ty::{SysTypeName, TypeName, TypeTag};

/// A function type, with inference allowed
#[derive(Ord, PartialOrd, Eq, PartialEq)]
struct TypeFn {
    qualifier: Option<TypeName>,
    generics: Generics,
    params: Vec<TypeTag>,
    ret_ty: TypeTag,
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
    fn builtin(
        &mut self,
        ident: &str,
        qualifier: SysTypeName,
        generics: Generics,
        params: Vec<TypeTag>,
        ret_ty: TypeTag,
    ) {
        let name = FuncName::Usr(UsrFuncName::intrinsic(ident));
        let func = TypeFn {
            qualifier: Some(TypeName::Sys(qualifier)),
            generics,
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
        use SysTypeName as Q;
        use TypeTag::*;

        // shorthands for generic types
        let t = || Parameter(TypeParamName::intrinsic("T"));
        let k = || Parameter(TypeParamName::intrinsic("K"));
        let v = || Parameter(TypeParamName::intrinsic("V"));
        let box_t = || Cloak(t().into());
        let seq_t = || Seq(t().into());
        let set_t = || Set(t().into());
        let map_kv = || Map(k().into(), v().into());

        let na = || Generics::intrinsic(vec![]);
        let with_t = || Generics::intrinsic(vec![TypeParamName::intrinsic("T")]);
        let with_kv = || {
            Generics::intrinsic(vec![
                TypeParamName::intrinsic("K"),
                TypeParamName::intrinsic("V"),
            ])
        };

        // construct the database
        let mut db = Self::new();

        // logical operators
        db.builtin("not", Q::Boolean, na(), vec![Boolean], Boolean);
        db.builtin("and", Q::Boolean, na(), vec![Boolean, Boolean], Boolean);
        db.builtin("or", Q::Boolean, na(), vec![Boolean, Boolean], Boolean);
        db.builtin("xor", Q::Boolean, na(), vec![Boolean, Boolean], Boolean);

        // arithmetic
        db.builtin("add", Q::Integer, na(), vec![Integer, Integer], Integer);
        db.builtin("add", Q::Rational, na(), vec![Rational, Rational], Rational);

        db.builtin("sub", Q::Integer, na(), vec![Integer, Integer], Integer);
        db.builtin("sub", Q::Rational, na(), vec![Rational, Rational], Rational);

        db.builtin("mul", Q::Integer, na(), vec![Integer, Integer], Integer);
        db.builtin("mul", Q::Rational, na(), vec![Rational, Rational], Rational);

        db.builtin("div", Q::Integer, na(), vec![Integer, Integer], Integer);
        db.builtin("div", Q::Rational, na(), vec![Rational, Rational], Rational);

        db.builtin("rem", Q::Integer, na(), vec![Integer, Integer], Integer);

        // (in)equality operators
        db.builtin("eq", Q::Boolean, na(), vec![Boolean, Boolean], Boolean);
        db.builtin("eq", Q::Integer, na(), vec![Integer, Integer], Boolean);
        db.builtin("eq", Q::Rational, na(), vec![Rational, Rational], Boolean);
        db.builtin("eq", Q::Text, na(), vec![Text, Text], Boolean);
        db.builtin("eq", Q::Cloak, with_t(), vec![box_t(), box_t()], Boolean);
        db.builtin("eq", Q::Seq, with_t(), vec![seq_t(), seq_t()], Boolean);
        db.builtin("eq", Q::Set, with_t(), vec![set_t(), set_t()], Boolean);
        db.builtin("eq", Q::Map, with_kv(), vec![map_kv(), map_kv()], Boolean);
        db.builtin("eq", Q::Error, na(), vec![Error, Error], Boolean);

        db.builtin("ne", Q::Boolean, na(), vec![Boolean, Boolean], Boolean);
        db.builtin("ne", Q::Integer, na(), vec![Integer, Integer], Boolean);
        db.builtin("ne", Q::Rational, na(), vec![Rational, Rational], Boolean);
        db.builtin("ne", Q::Text, na(), vec![Text, Text], Boolean);
        db.builtin("ne", Q::Cloak, with_t(), vec![box_t(), box_t()], Boolean);
        db.builtin("ne", Q::Seq, with_t(), vec![seq_t(), seq_t()], Boolean);
        db.builtin("ne", Q::Set, with_t(), vec![set_t(), set_t()], Boolean);
        db.builtin("ne", Q::Map, with_kv(), vec![map_kv(), map_kv()], Boolean);
        db.builtin("ne", Q::Error, na(), vec![Error, Error], Boolean);

        // comparison operators
        db.builtin("lt", Q::Integer, na(), vec![Integer, Integer], Boolean);
        db.builtin("lt", Q::Rational, na(), vec![Rational, Rational], Rational);
        db.builtin("lt", Q::Text, na(), vec![Text, Text], Rational);

        db.builtin("le", Q::Integer, na(), vec![Integer, Integer], Boolean);
        db.builtin("le", Q::Rational, na(), vec![Rational, Rational], Rational);
        db.builtin("le", Q::Text, na(), vec![Text, Text], Rational);

        db.builtin("ge", Q::Integer, na(), vec![Integer, Integer], Boolean);
        db.builtin("ge", Q::Rational, na(), vec![Rational, Rational], Rational);

        db.builtin("gt", Q::Integer, na(), vec![Integer, Integer], Boolean);
        db.builtin("gt", Q::Rational, na(), vec![Rational, Rational], Rational);

        // cloaking
        db.builtin("shield", Q::Cloak, with_t(), vec![t()], box_t());
        db.builtin("reveal", Q::Cloak, with_t(), vec![box_t()], t());

        // collections
        db.builtin("empty", Q::Seq, with_t(), vec![], seq_t());
        db.builtin("empty", Q::Set, with_t(), vec![], set_t());
        db.builtin("empty", Q::Map, with_kv(), vec![], map_kv());

        db.builtin("length", Q::Seq, with_t(), vec![seq_t()], Integer);
        db.builtin("length", Q::Set, with_t(), vec![set_t()], Integer);
        db.builtin("length", Q::Map, with_kv(), vec![map_kv()], Integer);

        // seq
        db.builtin("append", Q::Seq, with_t(), vec![seq_t(), t()], seq_t());
        db.builtin(
            "at_unchecked",
            Q::Seq,
            with_t(),
            vec![seq_t(), Integer],
            t(),
        );
        db.builtin("includes", Q::Seq, with_t(), vec![seq_t(), t()], Boolean);

        // set
        db.builtin("insert", Q::Set, with_t(), vec![set_t(), t()], set_t());
        db.builtin("contains", Q::Set, with_t(), vec![set_t(), t()], Boolean);

        // map
        db.builtin(
            "put_unchecked",
            Q::Map,
            with_kv(),
            vec![map_kv(), k(), v()],
            map_kv(),
        );
        db.builtin("get_unchecked", Q::Map, with_kv(), vec![map_kv(), k()], v());
        db.builtin(
            "contains_key",
            Q::Map,
            with_kv(),
            vec![map_kv(), k()],
            Boolean,
        );

        // error
        db.builtin("fresh", Q::Error, na(), vec![], Error);
        db.builtin("merge", Q::Error, na(), vec![Error, Error], Error);

        // done
        db
    }

    /// Register a user-defined SMT type
    pub fn register_user_type(&mut self, name: &UsrTypeName, generics: &Generics) {
        let ty_params = generics.vec();
        let ty_tag = TypeTag::User(
            name.clone(),
            ty_params
                .iter()
                .map(|p| TypeTag::Parameter(p.clone()))
                .collect(),
        );

        let mut register = |builtin: &str| {
            let func = TypeFn {
                qualifier: Some(TypeName::Usr(name.clone())),
                generics: generics.clone(),
                params: vec![ty_tag.clone(), ty_tag.clone()],
                ret_ty: TypeTag::Boolean,
            };
            let inserted = self
                .methods
                .entry(FuncName::Usr(UsrFuncName::intrinsic(builtin)))
                .or_default()
                .insert(func);
            if !inserted {
                panic!("duplicated registration of smt type: {}::{}", name, builtin);
            }
        };

        register("eq");
        register("ne");
    }

    /// Register a user-defined function (spec or impl)
    pub fn register_user_func(&mut self, name: &UsrFuncName, sig: &FuncSig) {
        let params = sig.param_vec().iter().map(|(_, ty)| ty.clone()).collect();
        let ret_ty = sig.ret_ty().clone();

        // if the first parameter is a user-defined type, mark it as a qualifier
        let qualifier = match sig.param_vec().first() {
            Some((_, TypeTag::User(tn, _))) => Some(TypeName::Usr(tn.clone())),
            _ => None,
        };

        let func = TypeFn {
            qualifier,
            generics: sig.generics().clone(),
            params,
            ret_ty,
        };
        let inserted = self
            .methods
            .entry(FuncName::Usr(name.clone()))
            .or_default()
            .insert(func);
        if !inserted {
            panic!("duplicated registration of smt func: {}", name);
        }
    }

    /// Report number of entries in the database
    pub fn size(&self) -> usize {
        self.methods.len()
    }
}

/// Represents a type variable participating in type unification
#[derive(Clone, Eq, PartialEq)]
pub struct TypeVar(usize);

/// Like `TypeTag`, but allows type variable for type unification
#[derive(Clone, Eq, PartialEq)]
pub enum TypeRef {
    /// variable
    Var(TypeVar),
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
    User(UsrTypeName, Vec<TypeRef>),
    /// parameter
    Parameter(TypeParamName),
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
            TypeTag::User(name, tags) => {
                Self::User(name.clone(), tags.iter().map(|t| t.into()).collect())
            }
            TypeTag::Parameter(name) => Self::Parameter(name.clone()),
        }
    }
}

/// Context manager for type unification
pub struct TypeUnifier {
    /// holds the set of possible candidates associated with each type parameter
    params: BTreeMap<usize, Option<BTreeSet<TypeTag>>>,
}

impl TypeUnifier {
    /// Create with an empty type unification context
    pub fn new() -> Self {
        Self {
            params: BTreeMap::new(),
        }
    }

    /// Make a new type parameter
    pub fn mk_var(&mut self) -> TypeVar {
        let id = self.params.len();
        let existing = self.params.insert(id, None);
        assert!(existing.is_none());
        TypeVar(id)
    }
}
