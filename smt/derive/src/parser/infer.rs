use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

use anyhow::{anyhow, bail};

use crate::parser::expr::{Expr, Op};
use crate::parser::func::FuncSig;
use crate::parser::generics::Generics;
use crate::parser::intrinsics::Intrinsic;
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
    methods: BTreeMap<UsrFuncName, BTreeSet<TypeFn>>,
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
        let name = UsrFuncName::intrinsic(ident);
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
                .entry(UsrFuncName::intrinsic(builtin))
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
        let inserted = self.methods.entry(name.clone()).or_default().insert(func);
        if !inserted {
            panic!("duplicated registration of smt func: {}", name);
        }
    }

    /// Get candidates given a function name
    pub fn get_inference(
        &self,
        unifier: &mut TypeUnifier,
        name: &UsrFuncName,
        qualifier: Option<&TypeName>,
        ty_args_opt: Option<&[TypeTag]>,
        args: Vec<Expr>,
        rval: &TypeRef,
    ) -> anyhow::Result<(Op, TypeRef)> {
        let mut suitable = None;
        for candidate in self
            .methods
            .get(name)
            .ok_or_else(|| anyhow!("no such method"))?
        {
            // early filtering
            match (candidate.qualifier.as_ref(), qualifier) {
                (None, Some(_)) => {
                    continue;
                }
                (Some(n1), Some(n2)) if n1 != n2 => {
                    continue;
                }
                _ => (),
            }
            if candidate.params.len() != args.len() {
                continue;
            }

            // use a probing unifier to check
            let mut probing = unifier.clone();

            // collect the type param substitution
            let ty_params = candidate.generics.vec();
            let param_substitutes: BTreeMap<_, _> = match ty_args_opt {
                None => ty_params
                    .into_iter()
                    .map(|name| (name, TypeRef::Var(probing.mk_var())))
                    .collect(),
                Some(ty_args) => {
                    if ty_params.len() != ty_args.len() {
                        continue;
                    }
                    ty_params
                        .into_iter()
                        .zip(ty_args.iter())
                        .map(|(name, ty_arg)| (name, ty_arg.into()))
                        .collect()
                }
            };

            // substitute types in function signature
            let params = candidate
                .params
                .iter()
                .map(|t| TypeRef::substitute_params(t, &param_substitutes))
                .collect::<anyhow::Result<Vec<_>>>()?;
            let ret_ty = TypeRef::substitute_params(&candidate.ret_ty, &param_substitutes)?;

            // unify all types
            let mut unified_args = vec![];
            for (param_ty, arg) in params.iter().zip(args.iter()) {
                let unified = probing.unify(param_ty, arg.ty())?;
                unified_args.push(unified);
            }
            let unified_ret = probing.unify(&ret_ty, rval)?;

            // if all types unifies, mark this candidate as viable
            if suitable.is_some() {
                bail!("more than one candidate match the method call");
            }
            suitable = Some((candidate, probing, unified_args, unified_ret));
        }

        // check that we have a match
        let (candidate, probing, unified_args, unified_ret) = match suitable {
            None => bail!("no candidates matches the method call"),
            Some(matched) => matched,
        };

        // override the unifier
        *unifier = probing;

        // assign the operation
        let op = match candidate.qualifier.as_ref() {
            None | Some(TypeName::Usr(_)) => {
                // must be a user-defined function
                Op::Procedure {
                    name: name.clone(),
                    args,
                }
            }
            Some(TypeName::Sys(tname)) => {
                use Intrinsic::*;
                use SysTypeName::*;

                let intrinsic = match (tname, name.as_ref()) {
                    // boolean
                    (Boolean, "not") => {
                        let a1 = Self::unpack_expr_1(args, unified_args)?;
                        Not(a1)
                    }
                    (Boolean, "and") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        And(a1, a2)
                    }
                    (Boolean, "or") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        Or(a1, a2)
                    }
                    (Boolean, "xor") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        Xor(a1, a2)
                    }
                    // integer
                    (Integer, "eq") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntEq(a1, a2)
                    }
                    (Integer, "ne") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntNe(a1, a2)
                    }
                    (Integer, "lt") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntLt(a1, a2)
                    }
                    (Integer, "le") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntLe(a1, a2)
                    }
                    (Integer, "ge") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntGe(a1, a2)
                    }
                    (Integer, "gt") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntGt(a1, a2)
                    }
                    (Integer, "add") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntAdd(a1, a2)
                    }
                    (Integer, "sub") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntSub(a1, a2)
                    }
                    (Integer, "mul") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntMul(a1, a2)
                    }
                    (Integer, "div") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntDiv(a1, a2)
                    }
                    (Integer, "rem") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        IntRem(a1, a2)
                    }
                    // rational
                    (Rational, "eq") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        NumEq(a1, a2)
                    }
                    (Rational, "ne") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        NumNe(a1, a2)
                    }
                    (Rational, "lt") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        NumLt(a1, a2)
                    }
                    (Rational, "le") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        NumLe(a1, a2)
                    }
                    (Rational, "ge") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        NumGe(a1, a2)
                    }
                    (Rational, "gt") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        NumGt(a1, a2)
                    }
                    (Rational, "add") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        NumAdd(a1, a2)
                    }
                    (Rational, "sub") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        NumSub(a1, a2)
                    }
                    (Rational, "mul") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        NumMul(a1, a2)
                    }
                    (Rational, "div") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        NumDiv(a1, a2)
                    }
                    // text
                    (Text, "eq") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        StrEq(a1, a2)
                    }
                    (Text, "ne") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        StrNe(a1, a2)
                    }
                    (Text, "lt") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        StrLt(a1, a2)
                    }
                    (Text, "le") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        StrLe(a1, a2)
                    }
                    // cloak
                    (Cloak, "shield") => {
                        let a1 = Self::unpack_expr_1(args, unified_args)?;
                        BoxShield(a1)
                    }
                    (Cloak, "reveal") => {
                        let a1 = Self::unpack_expr_1(args, unified_args)?;
                        BoxReveal(a1)
                    }
                    // seq
                    (Seq, "empty") => bail!("[invariant] unexpected"),
                    (Seq, "length") => {
                        let a1 = Self::unpack_expr_1(args, unified_args)?;
                        SeqLength(a1)
                    }
                    (Seq, "append") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        SeqAppend(a1, a2)
                    }
                    (Seq, "at_unchecked") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        SeqAt(a1, a2)
                    }
                    (Seq, "includes") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        SeqIncludes(a1, a2)
                    }
                    // set
                    (Set, "empty") => bail!("[invariant] unexpected"),
                    (Set, "length") => {
                        let a1 = Self::unpack_expr_1(args, unified_args)?;
                        SetLength(a1)
                    }
                    (Set, "insert") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        SetInsert(a1, a2)
                    }
                    (Set, "contains") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        SetContains(a1, a2)
                    }
                    // map
                    (Map, "empty") => bail!("[invariant] unexpected"),
                    (Map, "length") => {
                        let a1 = Self::unpack_expr_1(args, unified_args)?;
                        MapLength(a1)
                    }
                    (Map, "put_unchecked") => {
                        let (a1, a2, a3) = Self::unpack_expr_3(args, unified_args)?;
                        MapPut(a1, a2, a3)
                    }
                    (Map, "get_unchecked") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        MapGet(a1, a2)
                    }
                    (Map, "contains_key") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        MapContainsKey(a1, a2)
                    }
                    // error
                    (Error, "fresh") => bail!("[invariant] unexpected"),
                    (Error, "merge") => {
                        let (a1, a2) = Self::unpack_expr_2(args, unified_args)?;
                        ErrMerge(a1, a2)
                    }
                    // should have exhausted all
                    _ => bail!("[invariant] no such intrinsic"),
                };
                Op::Intrinsic(intrinsic)
            }
            Some(TypeName::Param(_)) => bail!("[invariant] type parameter as qualifier"),
        };

        // return the operation
        Ok((op, unified_ret))
    }

    /// Utility to unpack 1 argument
    fn unpack_expr_1(exprs: Vec<Expr>, tys: Vec<TypeRef>) -> anyhow::Result<Expr> {
        assert_eq!(exprs.len(), tys.len());
        let mut iter = exprs.into_iter().zip(tys);
        let e1 = match iter.next() {
            None => bail!("expect 1 argument"),
            Some((mut e, t)) => {
                e.set_ty(t);
                e
            }
        };
        if iter.next().is_some() {
            bail!("expect 1 argument");
        }
        Ok(e1)
    }

    /// Utility to unpack 2 arguments
    fn unpack_expr_2(exprs: Vec<Expr>, tys: Vec<TypeRef>) -> anyhow::Result<(Expr, Expr)> {
        assert_eq!(exprs.len(), tys.len());
        let mut iter = exprs.into_iter().zip(tys);
        let e1 = match iter.next() {
            None => bail!("expect 2 arguments"),
            Some((mut e, t)) => {
                e.set_ty(t);
                e
            }
        };
        let e2 = match iter.next() {
            None => bail!("expect 2 arguments"),
            Some((mut e, t)) => {
                e.set_ty(t);
                e
            }
        };
        if iter.next().is_some() {
            bail!("expect 2 arguments");
        }
        Ok((e1, e2))
    }

    /// Utility to unpack 3 arguments
    fn unpack_expr_3(exprs: Vec<Expr>, tys: Vec<TypeRef>) -> anyhow::Result<(Expr, Expr, Expr)> {
        assert_eq!(exprs.len(), tys.len());
        let mut iter = exprs.into_iter().zip(tys);
        let e1 = match iter.next() {
            None => bail!("expect 3 arguments"),
            Some((mut e, t)) => {
                e.set_ty(t);
                e
            }
        };
        let e2 = match iter.next() {
            None => bail!("expect 3 arguments"),
            Some((mut e, t)) => {
                e.set_ty(t);
                e
            }
        };
        let e3 = match iter.next() {
            None => bail!("expect 3 arguments"),
            Some((mut e, t)) => {
                e.set_ty(t);
                e
            }
        };
        if iter.next().is_some() {
            bail!("expect 3 arguments");
        }
        Ok((e1, e2, e3))
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

impl TypeRef {
    /// Apply type parameter substitution
    pub fn substitute_params(
        tag: &TypeTag,
        substitute: &BTreeMap<TypeParamName, TypeRef>,
    ) -> anyhow::Result<TypeRef> {
        let updated = match tag {
            TypeTag::Boolean => TypeRef::Boolean,
            TypeTag::Integer => TypeRef::Integer,
            TypeTag::Rational => TypeRef::Rational,
            TypeTag::Text => TypeRef::Text,
            TypeTag::Cloak(sub) => TypeRef::Cloak(Self::substitute_params(sub, substitute)?.into()),
            TypeTag::Seq(sub) => TypeRef::Seq(Self::substitute_params(sub, substitute)?.into()),
            TypeTag::Set(sub) => TypeRef::Set(Self::substitute_params(sub, substitute)?.into()),
            TypeTag::Map(key, val) => TypeRef::Map(
                Self::substitute_params(key, substitute)?.into(),
                Self::substitute_params(val, substitute)?.into(),
            ),
            TypeTag::Error => TypeRef::Error,
            TypeTag::User(name, args) => TypeRef::User(
                name.clone(),
                args.iter()
                    .map(|t| Self::substitute_params(t, substitute))
                    .collect::<anyhow::Result<_>>()?,
            ),
            TypeTag::Parameter(name) => substitute
                .get(name)
                .ok_or_else(|| anyhow!("no such type parameter"))?
                .clone(),
        };
        Ok(updated)
    }
}

/// An equivalence group of type variables
#[derive(Clone)]
struct TypeEquivGroup {
    vars: BTreeSet<usize>,
    ty: Option<TypeRef>,
}

/// Context manager for type unification
#[derive(Clone)]
pub struct TypeUnifier {
    /// holds the set of possible candidates associated with each type parameter
    params: BTreeMap<usize, usize>,
    /// hold the equivalence groups
    groups: Vec<TypeEquivGroup>,
}

impl TypeUnifier {
    /// Create with an empty type unification context
    pub fn new() -> Self {
        Self {
            params: BTreeMap::new(),
            groups: vec![],
        }
    }

    /// Make a new type parameter
    pub fn mk_var(&mut self) -> TypeVar {
        let var_id = self.params.len();

        // assign a fresh equivalence group to the type variable
        let group = TypeEquivGroup {
            vars: std::iter::once(var_id).collect(),
            ty: None,
        };
        let group_index = self.groups.len();

        // register the param and the gropu
        self.groups.push(group);
        let existing = self.params.insert(var_id, group_index);
        assert!(existing.is_none());

        // done
        TypeVar(var_id)
    }

    /// merge the type constrains
    fn merge_group(&mut self, l: &TypeVar, h: &TypeVar) -> anyhow::Result<TypeRef> {
        let idx_l = *self.params.get(&l.0).unwrap();
        let idx_h = *self.params.get(&h.0).unwrap();

        // obtain groups
        let mut group_l = self.groups.get(idx_l).unwrap().clone();
        let group_h = self.groups.get(idx_h).unwrap().clone();

        // unify the equivalence set
        if !group_l.vars.is_disjoint(&group_h.vars) {
            // this is an invariant violation
            bail!("[invariant] non-disjoint equivalence set");
        }
        group_l.vars.extend(group_h.vars);

        // check whether they unity to the same type, if any
        let inferred = match (group_l.ty.as_ref(), group_h.ty.as_ref()) {
            (None, None) => {
                // none of the groups have type inferred, return the type var with a lower index
                TypeRef::Var(l.clone())
            }
            (Some(t_l), None) => {
                // the lower group already has a constraint
                t_l.clone()
            }
            (None, Some(t_h)) => {
                // propagate the type to the lower group
                let _ = group_l.ty.insert(t_h.clone());
                t_h.clone()
            }
            (Some(t_l), Some(t_h)) => {
                // further unity (refine) the types, also check for mismatches
                let unified = self.unify(t_l, t_h)?;
                let _ = group_l.ty.insert(unified.clone());
                unified
            }
        };

        // redirect the group for the type variable at a higher index
        self.groups.remove(idx_l);
        self.groups.insert(idx_l, group_l);
        *self.params.get_mut(&h.0).unwrap() = idx_l;

        // return the inferred type
        Ok(inferred)
    }

    /// Assign the constraint
    fn update_group(&mut self, v: &TypeVar, t: &TypeRef) -> anyhow::Result<TypeRef> {
        // obtain the group
        let idx = *self.params.get(&v.0).unwrap();
        let mut group = self.groups.get(idx).unwrap().clone();

        // decides on whether further unification is needed
        let inferred = match group.ty.as_ref() {
            None => {
                // propagate the type to the group
                let _ = group.ty.insert(t.clone());
                t.clone()
            }
            Some(e) => {
                // further unity (refine) the types, also check for mismatches
                let unified = self.unify(e, t)?;
                let _ = group.ty.insert(unified.clone());
                unified
            }
        };

        // reset the equivalence the group for the type variable at a lower index
        self.groups.remove(idx);
        self.groups.insert(idx, group);

        // return the inferred type
        Ok(inferred)
    }

    /// Unify two types
    pub fn unify(&mut self, lhs: &TypeRef, rhs: &TypeRef) -> anyhow::Result<TypeRef> {
        use TypeRef::*;

        let inferred = match (lhs, rhs) {
            // variable
            (Var(l), Var(r)) => match Ord::cmp(&l.0, &r.0) {
                Ordering::Equal => {
                    // no knowledge gain in this case
                    Var(l.clone())
                }
                Ordering::Less => self.merge_group(l, r)?,
                Ordering::Greater => self.merge_group(r, l)?,
            },
            (Var(l), _) => self.update_group(l, rhs)?,
            (_, Var(r)) => self.update_group(r, lhs)?,
            // concrete types
            (Boolean, Boolean) => Boolean,
            (Integer, Integer) => Integer,
            (Rational, Rational) => Rational,
            (Text, Text) => Text,
            (Error, Error) => Error,
            // variadic types
            (Cloak(sub_lhs), Cloak(sub_rhs)) => Cloak(self.unify(sub_lhs, sub_rhs)?.into()),
            (Seq(sub_lhs), Seq(sub_rhs)) => Seq(self.unify(sub_lhs, sub_rhs)?.into()),
            (Set(sub_lhs), Set(sub_rhs)) => Set(self.unify(sub_lhs, sub_rhs)?.into()),
            (Map(key_lhs, val_lhs), Map(key_rhs, val_rhs)) => Map(
                self.unify(key_lhs, key_rhs)?.into(),
                self.unify(val_lhs, val_rhs)?.into(),
            ),
            // user-define types
            (User(name_lhs, vars_lhs), User(name_rhs, vars_rhs)) => {
                if name_lhs != name_rhs {
                    bail!("type mismatch");
                }
                assert_eq!(vars_lhs.len(), vars_rhs.len());

                let mut new_vars = vec![];
                for (v_lhs, v_rhs) in vars_lhs.iter().zip(vars_rhs.iter()) {
                    new_vars.push(self.unify(v_lhs, v_rhs)?);
                }
                User(name_lhs.clone(), new_vars)
            }
            // type parameters
            (Parameter(name_lhs), Parameter(name_rhs)) => {
                if name_lhs != name_rhs {
                    bail!("type mismatch");
                }
                Parameter(name_lhs.clone())
            }
            // all other cases are considered mismatch
            _ => bail!("type mismatch"),
        };
        Ok(inferred)
    }
}
