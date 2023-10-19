use std::collections::BTreeMap;

use crate::parser::func::FuncSig;
use crate::parser::generics::Generics;
use crate::parser::name::{TypeParamName, UsrFuncName, UsrTypeName};
use crate::parser::ty::{SysTypeName, TypeTag};

/// A function type, with inference allowed
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct TypeFn {
    generics: Generics,
    params: Vec<TypeTag>,
    ret_ty: TypeTag,
}

impl TypeFn {
    /// Get the generics
    pub fn generics(&self) -> &Generics {
        &self.generics
    }

    /// Collect variables (in vec) declared in the parameter list
    pub fn params(&self) -> &[TypeTag] {
        &self.params
    }

    /// Get the return type
    pub fn ret_ty(&self) -> &TypeTag {
        &self.ret_ty
    }
}

impl From<&FuncSig> for TypeFn {
    fn from(sig: &FuncSig) -> Self {
        Self {
            generics: sig.generics().clone(),
            params: sig.param_vec().iter().map(|(_, ty)| ty.clone()).collect(),
            ret_ty: sig.ret_ty().clone(),
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
        let fn0 = |rty: TypeTag| TypeFn {
            generics: Generics::intrinsic(vec![]),
            params: vec![],
            ret_ty: rty,
        };

        let fn1 = |a0: TypeTag, rty: TypeTag| TypeFn {
            generics: Generics::intrinsic(vec![]),
            params: vec![a0],
            ret_ty: rty,
        };
        let fn1_arith = |t: TypeTag| fn1(t.clone(), t);

        let fn2 = |a0: TypeTag, a1: TypeTag, rty: TypeTag| TypeFn {
            generics: Generics::intrinsic(vec![]),
            params: vec![a0, a1],
            ret_ty: rty,
        };
        let fn2_arith = |t: TypeTag| fn2(t.clone(), t.clone(), t);
        let fn2_cmp = |t: TypeTag| fn2(t.clone(), t, Boolean);

        let t = || Parameter(TypeParamName::intrinsic("T"));
        let box_t = || Cloak(t().into());
        let seq_t = || Seq(t().into());
        let set_t = || Set(t().into());
        let generics_t = || Generics::intrinsic(vec![TypeParamName::intrinsic("T")]);

        let k = || Parameter(TypeParamName::intrinsic("K"));
        let v = || Parameter(TypeParamName::intrinsic("V"));
        let map_kv = || Map(k().into(), v().into());
        let generics_kv = || {
            Generics::intrinsic(vec![
                TypeParamName::intrinsic("K"),
                TypeParamName::intrinsic("V"),
            ])
        };

        let fn0_t = |rty: TypeTag| TypeFn {
            generics: generics_t(),
            params: vec![],
            ret_ty: rty,
        };
        let fn0_kv = |rty: TypeTag| TypeFn {
            generics: generics_kv(),
            params: vec![],
            ret_ty: rty,
        };

        let fn1_t = |a0: TypeTag, rty: TypeTag| TypeFn {
            generics: generics_t(),
            params: vec![a0],
            ret_ty: rty,
        };
        let fn1_kv = |a0: TypeTag, rty: TypeTag| TypeFn {
            generics: generics_kv(),
            params: vec![a0],
            ret_ty: rty,
        };

        let fn2_t = |a0: TypeTag, a1: TypeTag, rty: TypeTag| TypeFn {
            generics: generics_t(),
            params: vec![a0, a1],
            ret_ty: rty,
        };
        let fn2_kv = |a0: TypeTag, a1: TypeTag, rty: TypeTag| TypeFn {
            generics: generics_kv(),
            params: vec![a0, a1],
            ret_ty: rty,
        };
        let fn3_kv = |a0: TypeTag, a1: TypeTag, a2: TypeTag, rty: TypeTag| TypeFn {
            generics: generics_kv(),
            params: vec![a0, a1, a2],
            ret_ty: rty,
        };

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
        db.builtin("shield", Q::Cloak, fn1_t(t(), box_t()));
        db.builtin("reveal", Q::Cloak, fn1_t(box_t(), t()));
        // seq
        db.builtin("empty", Q::Seq, fn0_t(seq_t()));
        db.builtin("length", Q::Seq, fn1_t(seq_t(), Integer));
        db.builtin("append", Q::Seq, fn2_t(seq_t(), t(), seq_t()));
        db.builtin("at_unchecked", Q::Seq, fn2_t(seq_t(), Integer, t()));
        db.builtin("includes", Q::Seq, fn2_t(seq_t(), t(), Boolean));
        // set
        db.builtin("empty", Q::Set, fn0_t(set_t()));
        db.builtin("length", Q::Set, fn1_t(set_t(), Integer));
        db.builtin("insert", Q::Set, fn2_t(set_t(), t(), set_t()));
        db.builtin("contains", Q::Set, fn2_t(set_t(), t(), Boolean));
        // map
        db.builtin("empty", Q::Map, fn0_kv(map_kv()));
        db.builtin("length", Q::Map, fn1_kv(map_kv(), Integer));
        db.builtin(
            "put_unchecked",
            Q::Map,
            fn3_kv(map_kv(), k(), v(), map_kv()),
        );
        db.builtin("get_unchecked", Q::Map, fn2_kv(map_kv(), k(), v()));
        db.builtin("contains_key", Q::Map, fn2_kv(map_kv(), k(), Boolean));
        // error
        db.builtin("fresh", Q::Error, fn0(Error));
        db.builtin("merge", Q::Error, fn2_arith(Error));

        // done with the registration
        db
    }

    /// Register a user-defined function (spec or impl)
    pub fn register_user_func(&mut self, name: &UsrFuncName, sig: &FuncSig) {
        let func = TypeFn::from(sig);

        // if the first param is a user-defined type with correct generics, mark it as a qualifier
        let qualifier = match func.params.first() {
            Some(TypeTag::User(tn, ty_args)) => {
                let ty_params = sig.generics().vec();
                if ty_args.len() != ty_params.len() {
                    None
                } else if ty_params.iter().zip(ty_args).all(
                    |(param_name, tag)| matches!(tag, TypeTag::Parameter(n) if n == param_name),
                ) {
                    Some(tn.clone())
                } else {
                    None
                }
            }
            _ => None,
        };

        // register this function
        match self.unqualified.insert(name.clone(), func.clone()) {
            None => (),
            Some(_) => panic!("duplicated registration of user-defined function: {}", name),
        }

        if let Some(ty_name) = qualifier {
            match self
                .on_usr_type
                .entry(name.clone())
                .or_default()
                .insert(ty_name.clone(), func.clone())
            {
                None => (),
                Some(_) => panic!(
                    "duplicated registration of user-defined function: {}::{}",
                    ty_name, name
                ),
            }
        }
    }

    /// Lookup an intrinsic function
    pub fn lookup_intrinsic(
        &self,
        ty_name: &SysTypeName,
        fn_name: &UsrFuncName,
    ) -> Option<&TypeFn> {
        self.on_sys_type.get(fn_name).and_then(|s| s.get(ty_name))
    }

    /// Lookup a user-defined function with a user-defined type as potentially receiver
    pub fn lookup_with_usr_ty(
        &self,
        ty_name: &UsrTypeName,
        fn_name: &UsrFuncName,
    ) -> Option<&TypeFn> {
        self.on_usr_type.get(fn_name).and_then(|s| s.get(ty_name))
    }
}
