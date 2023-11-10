use std::collections::{BTreeMap, BTreeSet};

use anyhow::{bail, Result};

use crate::ir::fun::FunRegistry;
use crate::ir::sort::{SmtSortName, Sort, TypeRegistry};
use crate::parser::ctxt::{ASTContext, Refinement};
use crate::parser::generics::Generics;
use crate::parser::infer::TypeRef;
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
    pub ctxt: &'ctx ASTContext,
    /// type instantiation in the current context
    pub ty_inst: BTreeMap<TypeParamName, Sort>,
    /// the ir to be accumulated
    pub ir: &'a mut IRContext,
}

impl<'a, 'ctx: 'a> IRBuilder<'a, 'ctx> {
    /// Change the analysis context
    pub fn new(
        ctxt: &'ctx ASTContext,
        ty_inst: BTreeMap<TypeParamName, Sort>,
        ir: &'a mut IRContext,
    ) -> Self {
        Self { ctxt, ty_inst, ir }
    }

    /// Contextualize into a new builder
    pub fn derive(&mut self, generics: &Generics, ty_args: Vec<Sort>) -> Result<IRBuilder> {
        // prepare the builder for definition processing
        let ty_params = &generics.params;
        if ty_params.len() != ty_args.len() {
            bail!("generics mismatch");
        }

        let mut ty_inst = BTreeMap::new();
        for (param, arg) in ty_params.iter().zip(ty_args.iter()) {
            match ty_inst.insert(param.clone(), arg.clone()) {
                None => (),
                Some(_) => bail!("duplicated type parameter {}", param),
            }
        }

        Ok(IRBuilder::new(self.ctxt, ty_inst, self.ir))
    }

    /// Initialize it with a new refinement relation
    pub fn build(ctxt: &'ctx ASTContext, rel: &'ctx Refinement) -> Result<IRContext> {
        let mut ir = IRContext::new();

        // get the pair
        let fn_impl = ctxt.get_func(&rel.fn_impl);
        let fn_spec = ctxt.get_func(&rel.fn_spec);

        // assign uninterpreted sorts as type arguments for function type parameters
        let generics_impl = &fn_impl.head.generics.params;
        let generics_spec = &fn_spec.head.generics.params;
        if generics_impl != generics_spec {
            bail!("generics mismatch");
        }

        // type instantiation for both spec and impl
        let mut ty_args = vec![];
        // type arguments for IR builder context
        let mut ty_inst = BTreeMap::new();

        for ty_param in generics_impl {
            let smt_name = SmtSortName::new(ty_param);
            ir.undef_sorts.insert(smt_name.clone());

            let smt_sort = Sort::Uninterpreted(smt_name);
            match ty_inst.insert(ty_param.clone(), smt_sort) {
                None => (),
                Some(_) => bail!("duplicated type parameter {}", ty_param),
            }
            ty_args.push(TypeRef::Parameter(ty_param.clone()));
        }

        // initialize the builder
        let mut builder = IRBuilder::new(ctxt, ty_inst, &mut ir);

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

        // process the impl and spec pair
        builder.resolve_func(&rel.fn_impl, &ty_args)?;
        builder.resolve_func(&rel.fn_spec, &ty_args)?;

        // done
        Ok(ir)
    }
}
