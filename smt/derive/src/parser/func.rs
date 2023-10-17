use std::collections::{BTreeMap, BTreeSet};

use syn::{FnArg, Ident, PatType, Result, ReturnType, Signature};

use crate::parser::ctxt::ContextWithType;
use crate::parser::err::{bail_if_exists, bail_on};
use crate::parser::expr::Expr;
use crate::parser::generics::Generics;
use crate::parser::name::{ReservedIdent, UsrFuncName, UsrTypeName, VarName};
use crate::parser::ty::{CtxtForType, TypeTag};
use crate::parser::util::PatUtil;

/// Reserved function name
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum SysFuncName {
    From,
    Into,
    Eq,
    Ne,
}

impl ReservedIdent for SysFuncName {
    fn from_str(ident: &str) -> Option<Self> {
        let matched = match ident.to_string().as_str() {
            "from" => Self::From,
            "into" => Self::Into,
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
    Sys(SysFuncName),
    Usr(UsrFuncName),
}

impl FuncName {
    /// Try to convert an ident into a function name
    pub fn try_from(ident: &Ident) -> Result<Self> {
        let name = ident.to_string();
        let parsed = match SysFuncName::from_str(&name) {
            None => Self::Usr(ident.try_into()?),
            Some(n) => Self::Sys(n),
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
