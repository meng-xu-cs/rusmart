use std::collections::BTreeMap;

use itertools::Itertools;

use crate::ir::ctxt::IRBuilder;
use crate::ir::exp::{ExpBuilder, ExpRegistry};
use crate::ir::fun::FunSig;
use crate::ir::index::{ExpId, UsrAxiomId};
use crate::ir::name::{Symbol, UsrAxiomName};
use crate::ir::sort::Sort;
use crate::parser::func::{Axiom, FuncSig};
use crate::parser::infer::TypeRef;
use crate::parser::name::AxiomName;

/// Axiom
pub struct Predicate {
    /// parameters
    pub params: Vec<(Symbol, Sort)>,
    /// body definition
    pub body_reg: ExpRegistry,
    /// body expression id
    pub body_exp: ExpId,
}

/// A registry of axioms involved
pub struct AxiomRegistry {
    /// a map from axiom instantiations to axiom id
    lookup: BTreeMap<UsrAxiomName, BTreeMap<Vec<Sort>, UsrAxiomId>>,
    /// a map for the actual axioms
    axioms: BTreeMap<UsrAxiomId, Predicate>,
}

impl AxiomRegistry {
    /// Initialize an empty registry
    pub fn new() -> Self {
        Self {
            lookup: BTreeMap::new(),
            axioms: BTreeMap::new(),
        }
    }

    /// Get the index given a name and instantiation
    fn get_index(&self, name: &UsrAxiomName, inst: &[Sort]) -> Option<UsrAxiomId> {
        self.lookup.get(name)?.get(inst).copied()
    }

    /// Create a place holder in the registry with function instance information, panics if already exists
    fn create(&mut self, name: UsrAxiomName, inst: Vec<Sort>) -> UsrAxiomId {
        let idx = UsrAxiomId {
            index: self.lookup.values().map(|v| v.len()).sum::<usize>(),
        };
        let existing = self.lookup.entry(name).or_default().insert(inst, idx);
        if existing.is_some() {
            panic!("axiom instance already registered");
        }
        idx
    }

    /// Register a definition to the registry
    fn register(&mut self, idx: UsrAxiomId, def: Predicate) {
        let existing = self.axioms.insert(idx, def);
        if existing.is_some() {
            panic!("axiom already registered");
        }
    }
}

impl<'a, 'ctx: 'a> IRBuilder<'a, 'ctx> {
    /// Register the axiom
    pub fn register_axiom(&mut self, name: &AxiomName, inst: &[TypeRef]) -> UsrAxiomId {
        let ident = name.into();
        let ty_args = self.resolve_type_ref_vec(inst);

        // panic if we have already processed the axiom
        if self.ir.axiom_registry.get_index(&ident, &ty_args).is_some() {
            panic!(
                "axiom already processed: {}<{}>",
                name,
                inst.iter().format(",")
            );
        }

        // register the instance and get the index
        let idx = self.ir.axiom_registry.create(ident, ty_args.clone());

        // unpack the def
        let Axiom {
            head:
                FuncSig {
                    generics,
                    params,
                    ret_ty,
                },
            body,
        } = self.ctxt.get_axiom(name);

        // prepare the builder for definition processing
        let mut builder = self.derive(generics, ty_args);

        // resolve type in function signatures
        let mut resolved_params = vec![];
        for (param_name, param_ty) in params {
            let param_sort = builder.resolve_type(&(param_ty.into()));
            resolved_params.push((param_name.into(), param_sort));
        }
        let resolved_ret_ty = builder.resolve_type(&(ret_ty.into()));

        let sig = FunSig {
            params: resolved_params,
            ret_ty: resolved_ret_ty,
        };

        // materialize the predicate
        let (exp_reg, exp_id) = ExpBuilder::materialize(builder, &sig, body);

        let predicate = Predicate {
            params: sig.params,
            body_reg: exp_reg,
            body_exp: exp_id,
        };
        self.ir.axiom_registry.register(idx, predicate);

        // done
        idx
    }
}
