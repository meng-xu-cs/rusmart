use std::collections::BTreeMap;

use anyhow::{bail, Result};

use crate::ir::sort::{SmtSortName, Sort, TypeRegistry, TypeRegistryHolder};
use crate::parser::ctxt::{ContextWithFunc, Refinement};
use crate::parser::infer::TypeRef;

/// A context manager for building around a refinement relation
pub struct IRBuilder<'a> {
    /// context provider
    ctxt: &'a ContextWithFunc,
    /// type registry
    ty_registry: TypeRegistry,
}

impl<'a> IRBuilder<'a> {
    /// Create a new IR builder
    pub fn new(ctxt: &'a ContextWithFunc) -> Self {
        Self {
            ctxt,
            ty_registry: TypeRegistry::new(),
        }
    }

    /// Initialize it with a new refinement relation
    pub fn build(mut self, rel: &Refinement) -> Result<()> {
        let Self {
            ctxt,
            mut ty_registry,
        } = self;

        // get the pair
        let (fn_impl, fn_spec) = self.ctxt.get_relation(rel);

        // assign uninterpreted sorts as type arguments for function type parameters
        let generics_impl = &fn_impl.head.generics.params;
        let generics_spec = &fn_spec.head.generics.params;
        if generics_impl != generics_spec {
            bail!("generics mismatch");
        }

        let mut ty_args = BTreeMap::new();
        for ty_param in generics_impl {
            let sort = Sort::Uninterpreted(SmtSortName::new(ty_param));
            match ty_args.insert(ty_param.clone(), sort) {
                None => (),
                Some(_) => bail!("duplicated type parameter {}", ty_param),
            }
        }

        // process function parameters
        let mut ty_holder = TypeRegistryHolder::new(ctxt, ty_args, &mut ty_registry);

        let params_impl = &fn_impl.head.params;
        let params_spec = &fn_impl.head.params;
        if params_impl.len() != params_spec.len() {
            bail!("parameter mismatch");
        }
        for ((_, param_impl), (_, param_spec)) in params_impl.iter().zip(params_spec) {
            if param_impl != param_spec {
                bail!("parameter mismatch");
            }
            ty_holder.resolve(&TypeRef::from(param_impl))?;
        }

        // done
        Ok(())
    }
}
