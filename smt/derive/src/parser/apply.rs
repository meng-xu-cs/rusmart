use std::collections::{BTreeMap, BTreeSet};

use anyhow::{bail, Result};

use crate::parser::expr::{CtxtForExpr, Expr, Op};
use crate::parser::func::FuncSig;
use crate::parser::generics::Generics;
use crate::parser::infer::{TIError, TSError, TypeRef, TypeUnifier};
use crate::parser::intrinsics::Intrinsic;
use crate::parser::name::{TypeParamName, UsrFuncName, UsrTypeName};
use crate::parser::path::GenericsInstPartial;
use crate::parser::ty::{SysTypeName, TypeName, TypeTag};

/// Marks whether this function is for impl or spec
#[derive(Copy, Clone)]
pub enum Kind {
    /// actual implementation
    Impl,
    /// formal specification
    Spec,
}

/// A function type, with inference allowed
pub struct TypeFn {
    pub kind: Kind,
    pub generics: Generics,
    pub params: Vec<TypeTag>,
    pub ret_ty: TypeTag,
}

impl TypeFn {
    /// Create a new function type from its signature
    pub fn new_from_sig(sig: &FuncSig, kind: Kind) -> Self {
        Self {
            kind,
            generics: sig.generics.clone(),
            params: sig.params.iter().map(|(_, ty)| ty.clone()).collect(),
            ret_ty: sig.ret_ty.clone(),
        }
    }
}

/// A database of function types
pub struct ApplyDatabase {
    /// user-defined function without a qualifier
    unqualified: BTreeMap<UsrFuncName, TypeFn>,
    /// user-defined function with a system-built type qualifier
    on_sys_type: BTreeMap<UsrFuncName, BTreeMap<SysTypeName, TypeFn>>,
    /// user-defined function with a user-defined type qualifier
    on_usr_type: BTreeMap<UsrFuncName, BTreeMap<UsrTypeName, TypeFn>>,
}

impl ApplyDatabase {
    /// Create an empty database
    fn new() -> Self {
        Self {
            unqualified: BTreeMap::new(),
            on_sys_type: BTreeMap::new(),
            on_usr_type: BTreeMap::new(),
        }
    }

    /// Register a built-in
    fn builtin(&mut self, fn_name: &str, ty_name: SysTypeName, sig: TypeFn) {
        match self
            .on_sys_type
            .entry(UsrFuncName::intrinsic(fn_name))
            .or_default()
            .insert(ty_name, sig)
        {
            None => (),
            Some(_) => {
                panic!("duplicated built-in: {}::{}", ty_name, fn_name);
            }
        }
    }

    /// Initialize the database with a function signature for intrinsics
    pub fn with_intrinsics() -> Self {
        use SysTypeName as Q;
        use TypeTag::*;

        // utility
        let empty = || Generics::intrinsic(vec![]);

        let fn0 = |rty: TypeTag| TypeFn {
            kind: Kind::Impl,
            generics: empty(),
            params: vec![],
            ret_ty: rty,
        };

        let fn1 = |a0: TypeTag, rty: TypeTag| TypeFn {
            kind: Kind::Impl,
            generics: empty(),
            params: vec![a0],
            ret_ty: rty,
        };
        let fn1_arith = |t: TypeTag| fn1(t.clone(), t);

        let fn2 = |a0: TypeTag, a1: TypeTag, rty: TypeTag| TypeFn {
            kind: Kind::Impl,
            generics: empty(),
            params: vec![a0, a1],
            ret_ty: rty,
        };
        let fn2_arith = |t: TypeTag| fn2(t.clone(), t.clone(), t);
        let fn2_cmp = |t: TypeTag| fn2(t.clone(), t, Boolean);

        let fn3 = |a0: TypeTag, a1: TypeTag, a2: TypeTag, rty: TypeTag| TypeFn {
            kind: Kind::Impl,
            generics: empty(),
            params: vec![a0, a1, a2],
            ret_ty: rty,
        };

        let t = || Parameter(TypeParamName::intrinsic("T"));
        let box_t = || Cloak(t().into());
        let seq_t = || Seq(t().into());
        let set_t = || Set(t().into());

        let k = || Parameter(TypeParamName::intrinsic("K"));
        let v = || Parameter(TypeParamName::intrinsic("V"));
        let map_kv = || Map(k().into(), v().into());

        // the population process
        let mut db = Self::new();

        // boolean
        db.builtin("not", Q::Boolean, fn1_arith(Boolean));
        db.builtin("and", Q::Boolean, fn2_arith(Boolean));
        db.builtin("or", Q::Boolean, fn2_arith(Boolean));
        db.builtin("xor", Q::Boolean, fn2_arith(Boolean));
        // integer
        db.builtin("add", Q::Integer, fn2_arith(Integer));
        db.builtin("sub", Q::Integer, fn2_arith(Integer));
        db.builtin("mul", Q::Integer, fn2_arith(Integer));
        db.builtin("div", Q::Integer, fn2_arith(Integer));
        db.builtin("rem", Q::Integer, fn2_arith(Integer));
        db.builtin("lt", Q::Integer, fn2_cmp(Integer));
        db.builtin("le", Q::Integer, fn2_cmp(Integer));
        db.builtin("ge", Q::Integer, fn2_cmp(Integer));
        db.builtin("gt", Q::Integer, fn2_cmp(Integer));
        // rational
        db.builtin("add", Q::Rational, fn2_arith(Rational));
        db.builtin("sub", Q::Rational, fn2_arith(Rational));
        db.builtin("mul", Q::Rational, fn2_arith(Rational));
        db.builtin("div", Q::Rational, fn2_arith(Rational));
        db.builtin("lt", Q::Rational, fn2_cmp(Rational));
        db.builtin("le", Q::Rational, fn2_cmp(Rational));
        db.builtin("ge", Q::Rational, fn2_cmp(Rational));
        db.builtin("gt", Q::Rational, fn2_cmp(Rational));
        // text
        db.builtin("lt", Q::Text, fn2_cmp(Text));
        db.builtin("le", Q::Text, fn2_cmp(Text));
        // cloak
        db.builtin("shield", Q::Cloak, fn1(t(), box_t()));
        db.builtin("reveal", Q::Cloak, fn1(box_t(), t()));
        // seq
        db.builtin("empty", Q::Seq, fn0(seq_t()));
        db.builtin("length", Q::Seq, fn1(seq_t(), Integer));
        db.builtin("append", Q::Seq, fn2(seq_t(), t(), seq_t()));
        db.builtin("at_unchecked", Q::Seq, fn2(seq_t(), Integer, t()));
        db.builtin("includes", Q::Seq, fn2(seq_t(), t(), Boolean));
        // set
        db.builtin("empty", Q::Set, fn0(set_t()));
        db.builtin("length", Q::Set, fn1(set_t(), Integer));
        db.builtin("insert", Q::Set, fn2(set_t(), t(), set_t()));
        db.builtin("contains", Q::Set, fn2(set_t(), t(), Boolean));
        // map
        db.builtin("empty", Q::Map, fn0(map_kv()));
        db.builtin("length", Q::Map, fn1(map_kv(), Integer));
        db.builtin("put_unchecked", Q::Map, fn3(map_kv(), k(), v(), map_kv()));
        db.builtin("get_unchecked", Q::Map, fn2(map_kv(), k(), v()));
        db.builtin("contains_key", Q::Map, fn2(map_kv(), k(), Boolean));
        // error
        db.builtin("fresh", Q::Error, fn0(Error));
        db.builtin("merge", Q::Error, fn2_arith(Error));

        // done with the registration
        db
    }

    /// Register a user-defined function (spec or impl)
    pub fn register_user_func(
        &mut self,
        name: &UsrFuncName,
        method: Option<&UsrFuncName>,
        sig: &FuncSig,
        kind: Kind,
    ) -> Result<()> {
        let func = TypeFn::new_from_sig(sig, kind);

        // try to register a method
        if let Some(method_name) = method {
            let (ty_name, ty_args) = match func.params.first() {
                None => bail!("no receiver argument"),
                Some(TypeTag::User(ty_name, ty_args)) => (ty_name, ty_args),
                Some(_) => bail!("the receiver argument is not a user-defined type"),
            };

            // for simplicity, require type generics be the first set of type parameters
            // TODO: relax this requirement
            let ty_params = &sig.generics.params;
            if ty_args.len() > ty_params.len() {
                bail!("the receiver argument take too many type arguments");
            }

            let mut ty_generics = BTreeSet::new();
            for (ty_arg, param_name) in ty_args.iter().zip(ty_params) {
                if !matches!(ty_arg, TypeTag::Parameter(n) if n == param_name) {
                    bail!("type parameter name mismatch");
                }
                ty_generics.insert(param_name.clone());
            }

            // reset the generics
            let method = TypeFn {
                kind: func.kind,
                generics: func.generics.filter(&ty_generics),
                params: func.params.clone(),
                ret_ty: func.ret_ty.clone(),
            };

            // all checked up
            match self
                .on_usr_type
                .entry(method_name.clone())
                .or_default()
                .insert(ty_name.clone(), method)
            {
                None => (),
                Some(_) => bail!(
                    "duplicated registration of user-defined function: {}::{}",
                    ty_name,
                    method_name
                ),
            }
        }

        // register this function as unqualified
        match self.unqualified.insert(name.clone(), func) {
            None => (),
            Some(_) => panic!("duplicated registration of user-defined function: {}", name),
        }

        // done
        Ok(())
    }

    /// Filter the return function type through kind
    fn filter_by_kind(ty: &TypeFn, kind: Kind) -> Option<&TypeFn> {
        match (kind, ty.kind) {
            (Kind::Impl, Kind::Impl) => Some(ty),
            (Kind::Impl, Kind::Spec) => None,
            (Kind::Spec, Kind::Impl | Kind::Spec) => Some(ty),
        }
    }

    /// Look-up a user function unqualified
    pub fn lookup_unqualified(&self, kind: Kind, fn_name: &UsrFuncName) -> Option<&TypeFn> {
        self.unqualified
            .get(fn_name)
            .and_then(|ty| Self::filter_by_kind(ty, kind))
    }

    /// Look-up a user function on a system type (a.k.a., an intrinsic function)
    pub fn lookup_usr_func_on_sys_type(
        &self,
        kind: Kind,
        ty_name: &SysTypeName,
        fn_name: &UsrFuncName,
    ) -> Option<&TypeFn> {
        self.on_sys_type
            .get(fn_name)
            .and_then(|s| s.get(ty_name))
            .and_then(|ty| Self::filter_by_kind(ty, kind))
    }

    /// Lookup a user-defined function with a user-defined type as potentially receiver
    pub fn lookup_usr_func_on_usr_type(
        &self,
        kind: Kind,
        ty_name: &UsrTypeName,
        fn_name: &UsrFuncName,
    ) -> Option<&TypeFn> {
        self.on_usr_type
            .get(fn_name)
            .and_then(|s| s.get(ty_name))
            .and_then(|ty| Self::filter_by_kind(ty, kind))
    }

    /// Get candidates given a function name
    pub fn query_with_inference<T: CtxtForExpr>(
        &self,
        unifier: &mut TypeUnifier,
        ctxt: &T,
        name: &UsrFuncName,
        inst: Option<&[TypeTag]>,
        args: Vec<Expr>,
        rval: &TypeRef,
    ) -> Result<Op> {
        // collect candidates
        let kind = ctxt.kind();
        let mut candidates = vec![];
        match self.on_sys_type.get(name) {
            None => (),
            Some(options) => candidates.extend(options.iter().filter_map(|(n, t)| {
                Self::filter_by_kind(t, kind).map(|t| (TypeName::Sys(*n), t))
            })),
        }
        match self.on_usr_type.get(name) {
            None => (),
            Some(options) => candidates.extend(options.iter().filter_map(|(n, t)| {
                Self::filter_by_kind(t, kind).map(|t| (TypeName::Usr(n.clone()), t))
            })),
        }

        // probe candidates
        let mut suitable = None;
        for (ty_name, fty) in candidates {
            // early filtering
            if fty.params.len() != args.len() {
                continue;
            }

            // instantiation
            let ty_inst = match &ty_name {
                TypeName::Sys(sys_name) => {
                    GenericsInstPartial::new_without_args(&sys_name.generics())
                }
                TypeName::Usr(usr_name) => GenericsInstPartial::new_without_args(
                    ctxt.get_type_generics(usr_name).expect("user-defined type"),
                ),
                TypeName::Param(_) => panic!("unexpected"),
            };
            let fn_inst = match inst {
                None => GenericsInstPartial::new_without_args(&fty.generics),
                Some(tags) => match GenericsInstPartial::try_with_args(&fty.generics, tags) {
                    None => continue,
                    Some(inst) => inst,
                },
            };

            // use a probing unifier to check
            let mut probing = unifier.clone();

            // for simplicity, require type generics be the first set of type parameters
            // TODO: relax this requirement
            let inst_full = match ty_inst
                .complete(&mut probing)
                .merge(&fn_inst.complete(&mut probing))
            {
                None => bail!("[invariant] conflicting type parameter name"),
                Some(inst) => inst,
            };

            // try to match the function
            let (params, ret_ty) = match probing.instantiate_func_ty(fty, &inst_full) {
                Ok(instantiated) => instantiated,
                Err(TSError::NoSuchParameter) => bail!("no such type parameter"),
            };

            // unify parameter types
            let mut unified = true;
            for (param_ty, arg) in params.iter().zip(args.iter()) {
                match probing.unify(param_ty, arg.ty()) {
                    Ok(Some(_)) => {
                        // type unified successfully, do nothing
                    }
                    Ok(None) => {
                        unified = false;
                        break;
                    }
                    Err(TIError::CyclicUnification) => bail!("cyclic type unification"),
                };
            }
            if !unified {
                continue;
            }

            // unify the return type
            match probing.unify(&ret_ty, rval) {
                Ok(Some(_)) => {
                    // type unified successfully, do nothing
                }
                Ok(None) => {
                    continue;
                }
                Err(TIError::CyclicUnification) => bail!("cyclic type unification"),
            }

            // if all types unifies, mark this candidate as viable
            if suitable.is_some() {
                bail!("more than one candidate match the method call");
            }
            suitable = Some((ty_name, probing, inst_full));
        }

        // check matching exists
        let (ty_name, probing, inst_full) = match suitable {
            None => bail!("no candidates matches the method call"),
            Some(matched) => matched,
        };

        // override the unifier
        *unifier = probing;

        // construct the operator and return it
        let op = match ty_name {
            TypeName::Sys(sys_name) => {
                let intrinsic = Intrinsic::new(&sys_name, name, inst_full.vec(), args)?;
                Op::Intrinsic(intrinsic)
            }
            TypeName::Usr(_) => Op::Procedure {
                name: name.clone(),
                inst: inst_full.vec(),
                args,
            },
            TypeName::Param(_) => panic!("unexpected"),
        };
        Ok(op)
    }
}
