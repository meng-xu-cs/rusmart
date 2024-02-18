use std::collections::BTreeMap;

use crate::ir::ctxt::IRBuilder;
use crate::ir::exp::{ExpBuilder, ExpRegistry};
use crate::ir::index::{ExpId, UsrFunId};
use crate::ir::name::{Symbol, UsrFunName};
use crate::ir::sort::Sort;
use crate::parser::func::{FuncDef, FuncSig};
use crate::parser::infer::TypeRef;
use crate::parser::name::UsrFuncName;

/// Function signature
#[derive(Clone)]
pub struct FunSig {
    /// parameters
    pub params: Vec<(Symbol, Sort)>,
    /// return value
    pub ret_ty: Sort,
}

/// Function definition
pub enum FunDef {
    /// imperatively defined function
    Defined(ExpRegistry, ExpId),
    /// uninterpreted and maybe axiomatized function
    Uninterpreted,
}

/// A registry of functions involved
#[derive(Default)]
pub struct FunRegistry {
    /// a map from user-defined functions and instantiations to function id
    pub lookup: BTreeMap<UsrFunName, BTreeMap<Vec<Sort>, UsrFunId>>,
    /// a map for function signatures
    sigs: BTreeMap<UsrFunId, FunSig>,
    /// a map for function definitions
    defs: BTreeMap<UsrFunId, FunDef>,
}

impl FunRegistry {
    /// Initialize an empty registry
    pub fn new() -> Self {
        Self {
            lookup: BTreeMap::new(),
            sigs: BTreeMap::new(),
            defs: BTreeMap::new(),
        }
    }

    /// Get the index given a name and instantiation
    fn get_index(&self, name: &UsrFunName, inst: &[Sort]) -> Option<UsrFunId> {
        self.lookup.get(name)?.get(inst).copied()
    }

    /// Create a place holder in the registry with function instance information, panics if already exists
    fn create(&mut self, name: UsrFunName, inst: Vec<Sort>) -> UsrFunId {
        let idx = UsrFunId {
            index: self.lookup.values().map(|v| v.len()).sum::<usize>(),
        };
        let existing = self.lookup.entry(name).or_default().insert(inst, idx);
        if existing.is_some() {
            panic!("function instance already registered");
        }
        idx
    }

    /// Register a signature to the registry
    fn register_sig(&mut self, idx: UsrFunId, sig: FunSig) {
        let existing = self.sigs.insert(idx, sig);
        if existing.is_some() {
            panic!("function signature already registered");
        }
    }

    /// Retrieve the function signature
    pub fn retrieve_sig(&self, idx: UsrFunId) -> &FunSig {
        self.sigs.get(&idx).expect("no such function id")
    }

    /// Register a definition to the registry
    fn register_def(&mut self, idx: UsrFunId, def: FunDef) {
        let existing = self.defs.insert(idx, def);
        if existing.is_some() {
            panic!("function definition already registered");
        }
    }

    /// Retrieve the function definition
    pub fn retrieve_def(&self, idx: UsrFunId) -> &FunDef {
        self.defs.get(&idx).expect("no such function id")
    }
}

impl<'a, 'ctx: 'a> IRBuilder<'a, 'ctx> {
    /// Register the function
    pub fn register_func(&mut self, fn_name: &UsrFuncName, ty_args: &[TypeRef]) -> UsrFunId {
        // derive the signature
        let name = fn_name.into();
        let ty_args = self.resolve_type_ref_vec(ty_args);

        // check if we have already processed the function
        match self.ir.fn_registry.get_index(&name, &ty_args) {
            None => (),
            Some(idx) => return idx,
        }

        // register the instance and get the index
        let idx = self.ir.fn_registry.create(name, ty_args.clone());

        // unpack the def
        let FuncDef {
            head:
                FuncSig {
                    generics,
                    params,
                    ret_ty,
                },
            body,
        } = self.ctxt.get_func(fn_name);

        // prepare the builder for definition processing
        let mut builder = self.derive(generics, ty_args);

        // resolve type in function signatures
        let mut resolved_params = vec![];
        for (param_name, param_ty) in params {
            let param_sort = builder.resolve_type(&(param_ty.into()));
            resolved_params.push((param_name.into(), param_sort));
        }
        let resolved_ret_ty = builder.resolve_type(&(ret_ty.into()));

        // register signature
        let sig = FunSig {
            params: resolved_params,
            ret_ty: resolved_ret_ty,
        };
        builder.ir.fn_registry.register_sig(idx, sig.clone());

        // materialize the entire function
        let def = match body.as_ref() {
            None => FunDef::Uninterpreted,
            Some(expr) => {
                let (exp_reg, exp_id) = ExpBuilder::materialize(builder, &sig, expr);
                FunDef::Defined(exp_reg, exp_id)
            }
        };

        // register the function definition
        self.ir.fn_registry.register_def(idx, def);

        // done
        idx
    }
}
