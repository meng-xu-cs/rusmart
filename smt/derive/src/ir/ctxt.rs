use std::collections::{BTreeMap, BTreeSet};

use log::trace;

use crate::ir::axiom::AxiomRegistry;
use crate::ir::fun::FunRegistry;
use crate::ir::sort::{SmtSortName, Sort, TypeRegistry};
use crate::parser::ctxt::{ASTContext, Refinement};
use crate::parser::generics::Generics;
use crate::parser::infer::TypeRef;
use crate::parser::name::{TypeParamName, UsrFuncName};
use crate::parser::ty::TypeTag;

/// A context for intermediate representation
pub struct IRContext {
    /// uninterpreted sorts
    pub undef_sorts: BTreeSet<SmtSortName>,
    /// type registry
    pub ty_registry: TypeRegistry,
    /// function registry
    pub fn_registry: FunRegistry,
    /// axiom registry
    pub axiom_registry: AxiomRegistry,
}

impl IRContext {
    /// Create an empty context
    fn new() -> Self {
        Self {
            undef_sorts: BTreeSet::new(),
            ty_registry: TypeRegistry::new(),
            fn_registry: FunRegistry::new(),
            axiom_registry: AxiomRegistry::new(),
        }
    }

    /// Reverse resolve a `Sort` to `TypeTag`
    fn reverse_sort(&self, sort: &Sort) -> TypeTag {
        match sort {
            Sort::Boolean => TypeTag::Boolean,
            Sort::Integer => TypeTag::Integer,
            Sort::Rational => TypeTag::Rational,
            Sort::Text => TypeTag::Text,
            Sort::Seq(sub) => TypeTag::Seq(self.reverse_sort(sub).into()),
            Sort::Set(sub) => TypeTag::Set(self.reverse_sort(sub).into()),
            Sort::Map(key, val) => {
                TypeTag::Map(self.reverse_sort(key).into(), self.reverse_sort(val).into())
            }
            Sort::Error => TypeTag::Error,
            Sort::User(sid) => {
                let (sort_name, sort_inst) = self.ty_registry.reverse_lookup(*sid);
                let inst = sort_inst.iter().map(|s| self.reverse_sort(s)).collect();
                match sort_name {
                    None => TypeTag::Pack(inst),
                    Some(name) => TypeTag::User(name, inst),
                }
            }
            Sort::Uninterpreted(name) => TypeTag::Parameter(name),
        }
    }

    /// List registered function instances
    fn function_instances(&self) -> Vec<(UsrFuncName, Vec<TypeTag>)> {
        for (name, insts) in &self.fn_registry.lookup {
            for inst in insts.keys() {
                todo!()
            }
        }
        todo!()
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
    fn new(
        ctxt: &'ctx ASTContext,
        ty_inst: BTreeMap<TypeParamName, Sort>,
        ir: &'a mut IRContext,
    ) -> Self {
        Self { ctxt, ty_inst, ir }
    }

    /// Contextualize into a new builder
    pub fn derive(&mut self, generics: &Generics, ty_args: Vec<Sort>) -> IRBuilder {
        let ty_params = &generics.params;
        if ty_params.len() != ty_args.len() {
            panic!("generics mismatch");
        }

        let mut ty_inst = BTreeMap::new();
        for (param, arg) in ty_params.iter().zip(ty_args.iter()) {
            match ty_inst.insert(param.clone(), arg.clone()) {
                None => (),
                Some(_) => panic!("duplicated type parameter {}", param),
            }
        }
        IRBuilder::new(self.ctxt, ty_inst, self.ir)
    }

    /// Initialize it with a new refinement relation
    pub fn build(ctxt: &'ctx ASTContext, rel: &'ctx Refinement) -> IRContext {
        let mut ir = IRContext::new();

        // get the pair
        let fn_impl = ctxt.get_func(&rel.fn_impl);
        let fn_spec = ctxt.get_func(&rel.fn_spec);

        // assign uninterpreted sorts as type arguments for function type parameters
        let generics_impl = &fn_impl.head.generics.params;
        let generics_spec = &fn_spec.head.generics.params;
        if generics_impl != generics_spec {
            panic!("generics mismatch");
        }

        // type instantiation for both spec and impl
        let mut ty_args = vec![];
        // type arguments for IR builder context
        let mut ty_inst = BTreeMap::new();

        for ty_param in generics_impl {
            let smt_name = SmtSortName::from(ty_param);
            ir.undef_sorts.insert(smt_name.clone());

            let smt_sort = Sort::Uninterpreted(smt_name);
            match ty_inst.insert(ty_param.clone(), smt_sort) {
                None => (),
                Some(_) => panic!("duplicated type parameter {}", ty_param),
            }
            ty_args.push(TypeRef::Parameter(ty_param.clone()));
        }
        trace!(
            "top-level type parameters: <{}>",
            ty_args
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(",")
        );

        // initialize the builder
        let mut builder = IRBuilder::new(ctxt, ty_inst, &mut ir);

        // sanity check on the refinement
        let params_impl = &fn_impl.head.params;
        let params_spec = &fn_spec.head.params;
        if params_impl.len() != params_spec.len() {
            panic!("parameter mismatch");
        }
        for ((param_name_impl, param_type_impl), (param_name_spec, param_type_spec)) in
            params_impl.iter().zip(params_spec)
        {
            if param_type_impl != param_type_spec {
                panic!(
                    "parameter ({} | {}) type mismatch",
                    param_name_impl, param_name_spec
                );
            }
        }
        if fn_impl.head.ret_ty != fn_spec.head.ret_ty {
            panic!("return type mismatch");
        }

        // process the impl and spec pair
        builder.register_func(&rel.fn_impl, &ty_args);
        builder.register_func(&rel.fn_spec, &ty_args);

        // pull in all relevant axioms
        let mut fixedpoint = true;
        loop {
            for (name, ty_args) in ir.function_instances() {
                for (axioms, inst) in ctxt.probe_related_axioms(&name, &ty_args) {
                    todo!()
                }
            }

            // exit the loop if we have reached a fixedpoint
            if fixedpoint {
                break;
            }
        }

        // done
        ir
    }
}
