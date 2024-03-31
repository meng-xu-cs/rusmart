use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter};

use syn::{
    Expr as Exp, ExprMacro, FnArg, Ident, Macro, MacroDelimiter, PatType, Result, ReturnType,
    Signature, Stmt,
};

use crate::parser::ctxt::ContextWithType;
use crate::parser::err::{bail_if_exists, bail_if_non_empty, bail_on};
use crate::parser::expr::Expr;
use crate::parser::generics::Generics;
use crate::parser::name::{ReservedIdent, UsrFuncName, UsrTypeName, VarName};
use crate::parser::ty::{CtxtForType, TypeTag};

/// Casting-related function names
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

impl Display for CastFuncName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::From => "from",
            Self::Into => "into",
        };
        f.write_str(name)
    }
}

/// System function names
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

impl Display for SysFuncName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::Eq => "eq",
            Self::Ne => "ne",
        };
        f.write_str(name)
    }
}

/// Reserved function names
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum ReservedFuncName {
    Clone,
    Default,
}

impl ReservedIdent for ReservedFuncName {
    fn from_str(ident: &str) -> Option<Self> {
        let matched = match ident.to_string().as_str() {
            "clone" => Self::Clone,
            "default" => Self::Default,
            _ => return None,
        };
        Some(matched)
    }
}

impl Display for ReservedFuncName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::Clone => "clone",
            Self::Default => "default",
        };
        f.write_str(name)
    }
}

/// A function name
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum FuncName {
    Cast(CastFuncName),
    Sys(SysFuncName),
    Usr(UsrFuncName),
    Reserved(ReservedFuncName),
}

impl FuncName {
    /// Try to convert an ident into a function name
    pub fn try_from(ident: &Ident) -> Result<Self> {
        let name = ident.to_string();
        let parsed = match ReservedFuncName::from_str(&name) {
            Some(n) => Self::Reserved(n),
            None => match CastFuncName::from_str(&name) {
                Some(n) => Self::Cast(n),
                None => match SysFuncName::from_str(&name) {
                    Some(n) => Self::Sys(n),
                    None => Self::Usr(ident.try_into()?),
                },
            },
        };
        Ok(parsed)
    }
}

impl Display for FuncName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cast(name) => name.fmt(f),
            Self::Sys(name) => name.fmt(f),
            Self::Usr(name) => name.fmt(f),
            Self::Reserved(name) => name.fmt(f),
        }
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
    pub generics: Generics,
    pub params: Vec<(VarName, TypeTag)>,
    pub ret_ty: TypeTag,
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

                    let name: VarName = pat.as_ref().try_into()?;
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

    /// Collect variables (in map) declared in the parameter list
    pub fn param_map(&self) -> BTreeMap<VarName, TypeTag> {
        self.params
            .iter()
            .map(|(name, ty)| (name.clone(), ty.clone()))
            .collect()
    }

    /// Test whether two signatures are type-compatible
    pub fn is_compatible(&self, sig: &FuncSig) -> bool {
        // for simplicity, require generics to use the same type parameter names
        // TODO: relax this requirement
        if self.generics.params != sig.generics.params {
            return false;
        }

        let lhs_params: Vec<_> = self.params.iter().map(|(_, t)| t).collect();
        let rhs_params: Vec<_> = sig.params.iter().map(|(_, t)| t).collect();
        if lhs_params != rhs_params {
            return false;
        }

        self.ret_ty == sig.ret_ty
    }
}

/// Function definition for impl
pub struct ImplFuncDef {
    pub head: FuncSig,
    pub body: Expr,
}

/// Function definition for spec
pub struct SpecFuncDef {
    pub head: FuncSig,
    pub body: Option<Expr>,
}

/// Function definition for both impl and spec
pub struct FuncDef {
    pub head: FuncSig,
    pub body: Option<Expr>,
}

impl From<ImplFuncDef> for FuncDef {
    fn from(def: ImplFuncDef) -> Self {
        let ImplFuncDef { head, body } = def;
        Self {
            head,
            body: Some(body),
        }
    }
}

impl From<SpecFuncDef> for FuncDef {
    fn from(def: SpecFuncDef) -> Self {
        let SpecFuncDef { head, body } = def;
        Self { head, body }
    }
}

/// Function definition for axiom
pub struct Axiom {
    pub head: FuncSig,
    pub body: Expr,
}

impl Axiom {
    /// Check whether the entire function body is `unimplemented!()`
    pub fn is_unimplemented(stmts: &[Stmt]) -> Result<bool> {
        if stmts.len() != 1 {
            return Ok(false);
        }
        let mac = match stmts.first().unwrap() {
            Stmt::Expr(Exp::Macro(ExprMacro { attrs: _, mac }), None) => mac,
            _ => return Ok(false),
        };

        let Macro {
            path,
            bang_token: _,
            delimiter,
            tokens,
        } = mac;
        if !path.is_ident("unimplemented") {
            return Ok(false);
        }

        // more rigorous checking
        if !matches!(delimiter, MacroDelimiter::Paren(_)) {
            bail_on!(mac, "invalid delimiter");
        }
        bail_if_non_empty!(tokens);

        // done
        Ok(true)
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
