use std::collections::{BTreeMap, BTreeSet};

use syn::{FnArg, Ident, PatType, Path, PathArguments, PathSegment, Result, ReturnType, Signature};

use crate::parser::ctxt::ContextWithType;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::expr::{CtxtForExpr, Expr};
use crate::parser::generics::Generics;
use crate::parser::infer::{TypeRef, TypeUnifier};
use crate::parser::name::{ReservedIdent, UsrFuncName, UsrTypeName, VarName};
use crate::parser::ty::{CtxtForType, TypeName, TypeTag};
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

/// An identifier for a function with type variables
#[derive(Clone)]
pub struct FuncPath {
    fn_name: UsrFuncName,
    ty_args: Vec<TypeRef>,
}

impl FuncPath {
    /// Extract a reference to a function from a path
    pub fn from_path<T: CtxtForExpr>(
        ctxt: &T,
        unifier: &mut TypeUnifier,
        path: &Path,
    ) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        let mut iter = segments.iter();

        // func
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "type name");
        let fn_name = ident.try_into()?;
        let ty_params_size = match ctxt.get_func_sig(&fn_name, ctxt.kind()) {
            None => bail_on!(ident, "no such function"),
            Some(sig) => sig.generics.len(),
        };

        let ty_args = match arguments {
            PathArguments::None => (0..ty_params_size)
                .map(|_| TypeRef::Var(unifier.mk_var()))
                .collect(),
            PathArguments::AngleBracketed(pack) => {
                let ty_args = TypeTag::from_args(ctxt, pack)?;
                if ty_args.len() != ty_params_size {
                    bail_on!(pack, "type argument number mismatch");
                }
                ty_args.iter().map(|t| t.into()).collect()
            }
            PathArguments::Parenthesized(_) => bail_on!(arguments, "invalid arguments"),
        };

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());

        // done
        Ok(Self { fn_name, ty_args })
    }

    /// Getter to the function name
    pub fn fn_name(&self) -> &UsrFuncName {
        &self.fn_name
    }

    /// Getter to the type arguments
    pub fn ty_args(&self) -> &[TypeRef] {
        &self.ty_args
    }
}

/// An identifier for a function with type variables
#[derive(Clone)]
pub struct QualifiedPath {
    ty_name: TypeName,
    fn_name: FuncName,
    ty_args: Option<Vec<TypeTag>>,
}

impl QualifiedPath {
    /// Extract a reference to a qualified call from a path
    pub fn from_path<T: CtxtForExpr>(ctxt: &T, path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        let mut iter = segments.iter();

        // type
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "type name");
        let ty_name = TypeName::try_from(ctxt.generics(), ident)?;
        match &ty_name {
            TypeName::Usr(name) => {
                if ctxt.get_type_def(name).is_none() {
                    bail_on!(ident, "no such type");
                }
            }
            _ => (),
        }
        if !matches!(arguments, PathArguments::None) {
            bail_on!(arguments, "unexpected type arguments");
        }

        // func
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "type name");
        let fn_name = FuncName::try_from(ident)?;
        match &fn_name {
            FuncName::Sys(_) => (),
            FuncName::Usr(name) => {
                if ctxt.get_func_sig(name, ctxt.kind()).is_none() {
                    bail_on!(ident, "no such function");
                }
            }
        }

        let ty_args = match arguments {
            PathArguments::None => None,
            PathArguments::AngleBracketed(pack) => Some(TypeTag::from_args(ctxt, pack)?),
            PathArguments::Parenthesized(_) => bail_on!(arguments, "invalid arguments"),
        };

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());

        // done
        Ok(Self {
            ty_name,
            fn_name,
            ty_args,
        })
    }

    /// Getter to the type name
    pub fn ty_name(&self) -> &TypeName {
        &self.ty_name
    }

    /// Getter to the func name
    pub fn fn_name(&self) -> &FuncName {
        &self.fn_name
    }

    /// Getter to the type arguments
    pub fn ty_args(&self) -> Option<&[TypeTag]> {
        self.ty_args.as_deref()
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
