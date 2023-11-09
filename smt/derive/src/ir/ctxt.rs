use std::collections::{BTreeMap, BTreeSet};

use anyhow::{bail, Result};

use crate::ir::fun::FunRegistry;
use crate::ir::sort::{SmtSortName, Sort, TypeRegistry};
use crate::parser::ctxt::{ContextWithFunc, Refinement};
use crate::parser::name::TypeParamName;

/// A context for intermediate representation
pub struct IRContext {
    /// uninterpreted sorts
    pub undef_sorts: BTreeSet<SmtSortName>,
    /// type registry
    pub ty_registry: TypeRegistry,
    /// function registry
    pub fn_registry: FunRegistry,
}

impl IRContext {
    /// Create an empty context
    fn new() -> Self {
        Self {
            undef_sorts: BTreeSet::new(),
            ty_registry: TypeRegistry::new(),
            fn_registry: FunRegistry::new(),
        }
    }
}

/// A context builder originated from a refinement relation
pub struct IRBuilder<'a, 'ctx: 'a> {
    /// context provider
    pub ctxt: &'ctx ContextWithFunc,
    /// type instantiation in the current context
    pub ty_args: BTreeMap<TypeParamName, Sort>,
    /// the ir to be accumulated
    pub ir: &'a mut IRContext,
}

impl<'a, 'ctx: 'a> IRBuilder<'a, 'ctx> {
    /// Change the analysis context
    pub fn new(
        ctxt: &'ctx ContextWithFunc,
        ty_args: BTreeMap<TypeParamName, Sort>,
        ir: &'a mut IRContext,
    ) -> Self {
        Self { ctxt, ty_args, ir }
    }

    /// Initialize it with a new refinement relation
    pub fn build(ctxt: &'ctx ContextWithFunc, rel: &'ctx Refinement) -> Result<IRContext> {
        let mut ir = IRContext::new();

        // get the pair
        let fn_impl = ctxt.get_impl(&rel.fn_impl);
        let fn_spec = ctxt.get_spec(&rel.fn_spec);

        // assign uninterpreted sorts as type arguments for function type parameters
        let generics_impl = &fn_impl.head.generics.params;
        let generics_spec = &fn_spec.head.generics.params;
        if generics_impl != generics_spec {
            bail!("generics mismatch");
        }

        let mut ty_args = BTreeMap::new();
        for ty_param in generics_impl {
            let name = SmtSortName::new(ty_param);
            let sort = Sort::Uninterpreted(name.clone());
            match ty_args.insert(ty_param.clone(), sort) {
                None => (),
                Some(_) => bail!("duplicated type parameter {}", ty_param),
            }
            ir.undef_sorts.insert(name);
        }

        // sanity check on the refinement
        let params_impl = &fn_impl.head.params;
        let params_spec = &fn_spec.head.params;
        if params_impl.len() != params_spec.len() {
            bail!("parameter mismatch");
        }
        for ((_, param_impl), (_, param_spec)) in params_impl.iter().zip(params_spec) {
            if param_impl != param_spec {
                bail!("parameter mismatch");
            }
        }
        if fn_impl.head.ret_ty != fn_spec.head.ret_ty {
            bail!("return type mismatch");
        }

        // initialize the builder
        let mut builder = IRBuilder::new(ctxt, ty_args, &mut ir);

        // process the impl and spec pair
        builder.process_impl(&rel.fn_impl)?;
        builder.process_spec(&rel.fn_spec)?;

        // done
        Ok(ir)
    }
}
