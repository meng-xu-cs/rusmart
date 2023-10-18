use std::collections::{BTreeMap, BTreeSet};

use syn::{FnArg, Ident, PatType, Result, ReturnType, Signature};

use crate::parser::ctxt::ContextWithType;
use crate::parser::err::{bail_if_exists, bail_on};
use crate::parser::expr::Expr;
use crate::parser::generics::Generics;
use crate::parser::name::{ReservedIdent, TypeParamName, UsrFuncName, UsrTypeName, VarName};
use crate::parser::ty::{CtxtForType, SysTypeName, TypeTag};
use crate::parser::util::PatUtil;

/// Reserved function name
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum CastFuncName {
    From,
    Into,
}

impl ReservedIdent for CastFuncName {
    fn from_str(ident: &str) -> Option<Self> {
        let matched = match ident.to_string().as_str() {
            "from" => Self::From,
            "into" => Self::Into,
            _ => return None,
        };
        Some(matched)
    }
}

/// Reserved function name
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum SysFuncName {
    Eq,
    Ne,
}

impl ReservedIdent for SysFuncName {
    fn from_str(ident: &str) -> Option<Self> {
        let matched = match ident.to_string().as_str() {
            "eq" => Self::Eq,
            "ne" => Self::Ne,
            _ => return None,
        };
        Some(matched)
    }
}

/// A function name
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum FuncName {
    Cast(CastFuncName),
    Sys(SysFuncName),
    Usr(UsrFuncName),
}

impl FuncName {
    /// Try to convert an ident into a function name
    pub fn try_from(ident: &Ident) -> Result<Self> {
        let name = ident.to_string();
        let parsed = match CastFuncName::from_str(&name) {
            Some(n) => Self::Cast(n),
            None => match SysFuncName::from_str(&name) {
                Some(n) => Self::Sys(n),
                None => Self::Usr(ident.try_into()?),
            },
        };
        Ok(parsed)
    }
}

/// A context provider for function signature parsing
struct FuncSigParseCtxt<'a> {
    ctxt: &'a ContextWithType,
    generics: &'a Generics,
}

impl CtxtForType for FuncSigParseCtxt<'_> {
    fn generics(&self) -> &Generics {
        self.generics
    }

    fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics> {
        self.ctxt.get_type_generics(name)
    }
}

/// Function signature
pub struct FuncSig {
    generics: Generics,
    params: Vec<(VarName, TypeTag)>,
    ret_ty: TypeTag,
}

impl FuncSig {
    /// Convert from a function signature
    pub fn from_sig(driver: &ContextWithType, sig: &Signature) -> Result<Self> {
        let Signature {
            constness,
            asyncness,
            unsafety,
            abi,
            fn_token: _,
            ident: _, // handled earlier
            generics,
            paren_token: _,
            inputs,
            variadic,
            output,
        } = sig;

        // should not appear
        bail_if_exists!(constness);
        bail_if_exists!(asyncness);
        bail_if_exists!(unsafety);
        bail_if_exists!(abi);
        bail_if_exists!(variadic);

        // generics
        let generics = Generics::from_generics(generics)?;
        let ctxt = FuncSigParseCtxt {
            ctxt: driver,
            generics: &generics,
        };

        // parameters
        let mut param_decls = vec![];
        let mut param_names = BTreeSet::new();
        for param in inputs {
            match param {
                FnArg::Receiver(recv) => bail_on!(recv, "unexpected self param"),
                FnArg::Typed(typed) => {
                    let PatType {
                        attrs: _,
                        pat,
                        colon_token: _,
                        ty,
                    } = typed;

                    let name: VarName = PatUtil::expect_name(pat)?;
                    if !param_names.insert(name.clone()) {
                        bail_on!(pat, "duplicated parameter name");
                    }

                    let ty = TypeTag::from_type(&ctxt, ty)?;
                    param_decls.push((name, ty));
                }
            }
        }

        // return type
        let ret_ty = match output {
            ReturnType::Default => bail_on!(sig, "expect return type"),
            ReturnType::Type(_, rty) => TypeTag::from_type(&ctxt, rty)?,
        };

        // done
        Ok(Self {
            generics,
            params: param_decls,
            ret_ty,
        })
    }

    /// Build a function signature for intrinsics
    pub fn intrinsic(ty_name: &SysTypeName, fn_name: &UsrFuncName) -> Option<Self> {
        use TypeTag::*;

        // utility
        let empty = |rty: TypeTag| Self {
            generics: Generics::intrinsic(vec![]),
            params: vec![],
            ret_ty: rty,
        };

        let unary = |a0: TypeTag, rty: TypeTag| Self {
            generics: Generics::intrinsic(vec![]),
            params: vec![(VarName::intrinsic(0), a0)],
            ret_ty: rty,
        };
        let unary_arith = |t: TypeTag| unary(t.clone(), t);

        let binary = |a0: TypeTag, a1: TypeTag, rty: TypeTag| Self {
            generics: Generics::intrinsic(vec![]),
            params: vec![(VarName::intrinsic(0), a0), (VarName::intrinsic(1), a1)],
            ret_ty: rty,
        };
        let binary_arith = |t: TypeTag| binary(t.clone(), t.clone(), t);
        let binary_cmp = |t: TypeTag| binary(t.clone(), t, Boolean);

        let t = || Parameter(TypeParamName::intrinsic("T"));
        let box_t = || Cloak(t().into());
        let seq_t = || Seq(t().into());
        let set_t = || Set(t().into());

        let k = || Parameter(TypeParamName::intrinsic("K"));
        let v = || Parameter(TypeParamName::intrinsic("V"));
        let map_kv = || Map(k().into(), v().into());

        let empty_t = |rty: TypeTag| Self {
            generics: Generics::intrinsic(vec![TypeParamName::intrinsic("T")]),
            params: vec![],
            ret_ty: rty,
        };
        let empty_kv = |rty: TypeTag| Self {
            generics: Generics::intrinsic(vec![
                TypeParamName::intrinsic("K"),
                TypeParamName::intrinsic("V"),
            ]),
            params: vec![],
            ret_ty: rty,
        };

        let unary_t = |a0: TypeTag, rty: TypeTag| Self {
            generics: Generics::intrinsic(vec![TypeParamName::intrinsic("T")]),
            params: vec![(VarName::intrinsic(0), a0)],
            ret_ty: rty,
        };
        let unary_kv = |a0: TypeTag, rty: TypeTag| Self {
            generics: Generics::intrinsic(vec![
                TypeParamName::intrinsic("K"),
                TypeParamName::intrinsic("V"),
            ]),
            params: vec![(VarName::intrinsic(0), a0)],
            ret_ty: rty,
        };

        let binary_t = |a0: TypeTag, a1: TypeTag, rty: TypeTag| Self {
            generics: Generics::intrinsic(vec![TypeParamName::intrinsic("T")]),
            params: vec![(VarName::intrinsic(0), a0), (VarName::intrinsic(1), a1)],
            ret_ty: rty,
        };
        let binary_kv = |a0: TypeTag, a1: TypeTag, rty: TypeTag| Self {
            generics: Generics::intrinsic(vec![
                TypeParamName::intrinsic("K"),
                TypeParamName::intrinsic("V"),
            ]),
            params: vec![(VarName::intrinsic(0), a0), (VarName::intrinsic(1), a1)],
            ret_ty: rty,
        };
        let ternary_kv = |a0: TypeTag, a1: TypeTag, a2: TypeTag, rty: TypeTag| Self {
            generics: Generics::intrinsic(vec![
                TypeParamName::intrinsic("K"),
                TypeParamName::intrinsic("V"),
            ]),
            params: vec![
                (VarName::intrinsic(0), a0),
                (VarName::intrinsic(1), a1),
                (VarName::intrinsic(2), a2),
            ],
            ret_ty: rty,
        };

        let sig = match (ty_name, fn_name.as_ref()) {
            // boolean
            (SysTypeName::Boolean, "not") => unary_arith(Boolean),
            (SysTypeName::Boolean, "and") => binary_arith(Boolean),
            (SysTypeName::Boolean, "or") => binary_arith(Boolean),
            (SysTypeName::Boolean, "xor") => binary_arith(Boolean),
            // integer
            (SysTypeName::Integer, "add") => binary_arith(Integer),
            (SysTypeName::Integer, "sub") => binary_arith(Integer),
            (SysTypeName::Integer, "mul") => binary_arith(Integer),
            (SysTypeName::Integer, "div") => binary_arith(Integer),
            (SysTypeName::Integer, "rem") => binary_arith(Integer),
            (SysTypeName::Integer, "lt") => binary_cmp(Integer),
            (SysTypeName::Integer, "le") => binary_cmp(Integer),
            (SysTypeName::Integer, "ge") => binary_cmp(Integer),
            (SysTypeName::Integer, "gt") => binary_cmp(Integer),
            // rational
            (SysTypeName::Rational, "add") => binary_arith(Rational),
            (SysTypeName::Rational, "sub") => binary_arith(Rational),
            (SysTypeName::Rational, "mul") => binary_arith(Rational),
            (SysTypeName::Rational, "div") => binary_arith(Rational),
            (SysTypeName::Rational, "lt") => binary_cmp(Rational),
            (SysTypeName::Rational, "le") => binary_cmp(Rational),
            (SysTypeName::Rational, "ge") => binary_cmp(Rational),
            (SysTypeName::Rational, "gt") => binary_cmp(Rational),
            // cloak
            (SysTypeName::Cloak, "shield") => unary_t(t(), box_t()),
            (SysTypeName::Cloak, "reveal") => unary_t(box_t(), t()),
            // seq
            (SysTypeName::Seq, "empty") => empty_t(seq_t()),
            (SysTypeName::Seq, "length") => unary_t(seq_t(), Integer),
            (SysTypeName::Seq, "append") => binary_t(seq_t(), t(), seq_t()),
            (SysTypeName::Seq, "at_unchecked") => binary_t(seq_t(), Integer, t()),
            (SysTypeName::Seq, "includes") => binary_t(seq_t(), t(), Boolean),
            // set
            (SysTypeName::Set, "empty") => empty_t(set_t()),
            (SysTypeName::Set, "length") => unary_t(set_t(), Integer),
            (SysTypeName::Set, "insert") => binary_t(set_t(), t(), set_t()),
            (SysTypeName::Set, "contains") => binary_t(set_t(), t(), Boolean),
            // map
            (SysTypeName::Map, "empty") => empty_kv(map_kv()),
            (SysTypeName::Map, "length") => unary_kv(map_kv(), Integer),
            (SysTypeName::Map, "put_unchecked") => ternary_kv(map_kv(), k(), v(), map_kv()),
            (SysTypeName::Seq, "get_unchecked") => binary_kv(map_kv(), k(), v()),
            (SysTypeName::Seq, "contains_key") => binary_kv(map_kv(), k(), Boolean),
            // error
            (SysTypeName::Error, "fresh") => empty(Error),
            (SysTypeName::Error, "merge") => binary_arith(Error),
            // others
            _ => return None,
        };
        Some(sig)
    }

    /// Get the generics
    pub fn generics(&self) -> &Generics {
        &self.generics
    }

    /// Collect variables (in map) declared in the parameter list
    pub fn param_map(&self) -> BTreeMap<VarName, TypeTag> {
        self.params
            .iter()
            .map(|(name, ty)| (name.clone(), ty.clone()))
            .collect()
    }

    /// Collect variables (in vec) declared in the parameter list
    pub fn param_vec(&self) -> &[(VarName, TypeTag)] {
        &self.params
    }

    /// Get the return type
    pub fn ret_ty(&self) -> &TypeTag {
        &self.ret_ty
    }
}

/// Function definition
pub struct FuncDef {
    sig: FuncSig,
    body: Expr,
}

impl FuncDef {
    /// Create a new function definition
    pub fn new(sig: FuncSig, body: Expr) -> Self {
        Self { sig, body }
    }
}
#[cfg(test)]
mod tests {
    use crate::parser::test::unit_test;

    unit_test!(plain, {
        #[smt_impl]
        fn foo() -> Boolean {
            false.into()
        }
    });

    unit_test!(with_generics, {
        #[smt_impl]
        fn foo<T: SMT>(t: T) -> T {
            t
        }
    });

    unit_test!(
        no_ret_ty,
        {
            #[smt_impl]
            fn foo() {}
        },
        "expect return type"
    );

    unit_test!(
        receiver,
        {
            #[smt_impl]
            fn foo(self) -> Boolean {
                false.into()
            }
        },
        "unexpected self param"
    );
}
