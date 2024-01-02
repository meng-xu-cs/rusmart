use std::collections::{BTreeMap, BTreeSet};

use log::trace;

use crate::ir::axiom::AxiomRegistry;
use crate::ir::fun::FunRegistry;
use crate::ir::mono::add_instantiation;
use crate::ir::name::SmtSortName;
use crate::ir::sort::{Sort, TypeRegistry};
use crate::parser::ctxt::{ASTContext, Refinement};
use crate::parser::generics::{Generics, PartialInst};
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
                    Some(name) => TypeTag::User(name.into(), inst),
                }
            }
            Sort::Uninterpreted(name) => TypeTag::Parameter(name.into()),
        }
    }

    /// List registered function instances
    fn reverse_function_instances(&self) -> Vec<(UsrFuncName, Vec<TypeTag>)> {
        let mut instances = vec![];
        for (name, insts) in &self.fn_registry.lookup {
            for inst in insts.keys() {
                let tags = inst.iter().map(|e| self.reverse_sort(e)).collect();
                instances.push((name.into(), tags));
            }
        }
        instances
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
            let smt_name = SmtSortName::new_func_param(&rel.fn_impl, ty_param);
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
        let mut builder = IRBuilder::new(ctxt, ty_inst.clone(), &mut ir);

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
        let mut relevant_axioms = BTreeMap::new();
        let mut uninterpreted_axiom_params = BTreeMap::new();

        let mut fixedpoint = true;
        loop {
            // consolidate all related axioms and their instantiations
            let mut batch = BTreeMap::new();
            for (func_name, func_inst) in ir.reverse_function_instances() {
                for (axiom_name, mut axiom_insts) in
                    ctxt.probe_related_axioms(&func_name, &func_inst)
                {
                    batch
                        .entry(axiom_name)
                        .or_insert_with(BTreeSet::new)
                        .append(&mut axiom_insts);
                }
            }

            // self-interference and register axioms
            for (name, insts) in batch {
                let axiom = ctxt.get_axiom(&name);
                let existing_insts = relevant_axioms
                    .entry(name.clone())
                    .or_insert_with(BTreeSet::new);

                let mut all_new_insts = vec![];
                for inst in insts {
                    let additions = add_instantiation(&axiom.head.generics, existing_insts, inst);
                    all_new_insts.extend(additions.into_iter());
                }

                // register axiom under each new instantiation
                for inst in all_new_insts {
                    // first collect unspecified types
                    for ty_arg_inst in &inst.args {
                        match ty_arg_inst {
                            PartialInst::Assigned(_) => (),
                            PartialInst::Unassigned(n) => {
                                let axiom_params_map = uninterpreted_axiom_params
                                    .entry(name.clone())
                                    .or_insert_with(BTreeMap::new);
                                if !axiom_params_map.contains_key(n) {
                                    let smt_name = SmtSortName::new_axiom_param(&name, n);
                                    ir.undef_sorts.insert(smt_name.clone());

                                    let smt_sort = Sort::Uninterpreted(smt_name);
                                    axiom_params_map.insert(n.clone(), smt_sort);
                                }
                            }
                        }
                    }

                    // specialized builder just for axiom args
                    let mut axiom_ty_builder = IRBuilder::new(ctxt, ty_inst.clone(), &mut ir);

                    // type instantiation for axiom
                    let mut axiom_ty_args = vec![];
                    // type arguments for IR builder context
                    let mut axiom_ty_inst = BTreeMap::new();

                    for (ty_param, ty_arg_inst) in
                        axiom.head.generics.params.iter().zip(inst.args.iter())
                    {
                        let (ty_arg_ref, ty_arg_sort) = match ty_arg_inst {
                            PartialInst::Assigned(t) => {
                                let tref = t.into();
                                let sort = axiom_ty_builder.resolve_type(&tref);
                                (tref, sort)
                            }
                            PartialInst::Unassigned(n) => {
                                let tref = TypeRef::Parameter(n.clone());
                                let sort = uninterpreted_axiom_params
                                    .get(&name)
                                    .and_then(|v| v.get(n))
                                    .expect("axiom type parameter variable created")
                                    .clone();
                                (tref, sort)
                            }
                        };
                        match axiom_ty_inst.insert(ty_param.clone(), ty_arg_sort) {
                            None => (),
                            Some(_) => panic!("duplicated type parameter {}", ty_param),
                        }
                        axiom_ty_args.push(ty_arg_ref);
                    }

                    // specialized builder for axiom body
                    let mut axiom_ty_builder = IRBuilder::new(ctxt, axiom_ty_inst, &mut ir);
                    axiom_ty_builder.register_axiom(&name, &axiom_ty_args);

                    // not reaching fixedpoint yet as long as we find a new monomorphization instance
                    fixedpoint = false;
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
