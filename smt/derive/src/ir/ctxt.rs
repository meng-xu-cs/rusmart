use anyhow::{bail, Result};
use std::collections::BTreeMap;

use crate::ir::sort::SmtSortName;
use crate::parser::ctxt::{ContextWithFunc, Refinement};
use crate::parser::name::TypeParamName;

/// A context manager for building around a refinement relation
pub struct IRBuilder {
    ty_args: BTreeMap<TypeParamName, SmtSortName>,
}

impl IRBuilder {
    /// Initialize it with a new refinement relation
    pub fn build(ctxt: &ContextWithFunc, rel: &Refinement) -> Result<Self> {
        let mut builder = Self {
            ty_args: BTreeMap::new(),
        };

        // get the pair
        let (fn_impl, fn_spec) = ctxt.get_relation(rel);

        // initialize uninterpreted sorts
        let generics_impl = &fn_impl.head.generics.params;
        let generics_spec = &fn_spec.head.generics.params;
        if generics_impl != generics_spec {
            bail!("generics mismatch");
        }
        for param_name in generics_impl {
            let sort_name = SmtSortName::new(param_name);
            match builder.ty_args.insert(param_name.clone(), sort_name) {
                None => (),
                Some(_) => bail!("duplicated type parameter {}", param_name),
            }
        }

        // done
        Ok(builder)
    }
}
