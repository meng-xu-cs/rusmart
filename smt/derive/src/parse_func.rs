use std::collections::BTreeSet;

use syn::{FnArg, Pat, PatIdent, PatType, Signature};

use crate::parse_ctxt::{bail_if_exists, bail_on, ContextWithType, VarName};
use crate::parse_type::TypeTag;

/// Function signature
pub struct FuncSig {
    params: Vec<(VarName, TypeTag)>,
    ret_ty: TypeTag,
}

impl FuncSig {
    /// Convert from a function signature
    fn from_sig(ctxt: &ContextWithType, sig: &Signature) -> Result<Self> {
        let Signature {
            constness,
            asyncness,
            unsafety,
            abi,
            fn_token: _,
            ident: _, // handled in context
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
        bail_if_exists!(&generics.lt_token);
        bail_if_exists!(&generics.gt_token);
        bail_if_exists!(variadic);

        // parse parameters
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

                    let name = VarName::from_pat(pat)?;
                    let tag = TypeTag::from_type(ctxt, ty)?;
                    param_decls.push((name, tag));
                }
            }
        }

        Ok(Self {
            params: param_decls,
        })
    }
}
