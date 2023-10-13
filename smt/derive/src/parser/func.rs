use std::collections::{BTreeMap, BTreeSet};

use syn::{FnArg, PatType, Result, ReturnType, Signature};

use crate::parser::err::{bail_if_exists, bail_on};
use crate::parser::name::VarName;
use crate::parser::ty::{CtxtForType, TypeTag};
use crate::parser::util::PatUtil;

/// Function signature
pub struct FuncSig {
    params: Vec<(VarName, TypeTag)>,
    ret_ty: TypeTag,
}

impl FuncSig {
    /// Convert from a function signature
    pub fn from_sig<T: CtxtForType>(ctxt: &T, sig: &Signature) -> Result<Self> {
        let Signature {
            constness,
            asyncness,
            unsafety,
            abi,
            fn_token: _,
            ident: _,    // handled earlier
            generics: _, // handled earlier
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

        // parameters
        let mut param_decls = vec![];
        let mut param_names = BTreeSet::new();
        for param in inputs {
            match param {
                FnArg::Receiver(recv) => bail_on!(recv, "unexpected self-style param"),
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

                    let ty = TypeTag::from_type(ctxt, ty)?;
                    param_decls.push((name, ty));
                }
            }
        }

        // return type
        let ret_ty = match output {
            ReturnType::Default => bail_on!(sig, "no return type"),
            ReturnType::Type(_, rty) => TypeTag::from_type(ctxt, rty)?,
        };

        // done
        Ok(Self {
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

    /// Collect variables (in vec) declared in the parameter list
    pub fn param_vec(&self) -> &[(VarName, TypeTag)] {
        &self.params
    }

    /// Get the return type
    pub fn ret_ty(&self) -> &TypeTag {
        &self.ret_ty
    }
}
