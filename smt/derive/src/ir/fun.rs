use std::collections::BTreeMap;

use anyhow::Result;

use crate::ir::ctxt::IRBuilder;
use crate::ir::exp::{ExpId, Expression, VarId, Variable};
use crate::ir::name::{index, name};
use crate::ir::sort::Sort;
use crate::parser::func::{FuncDef, FuncSig};
use crate::parser::infer::TypeRef;
use crate::parser::name::UsrFuncName;

name! {
    /// Name of a user-defined function
    UsrFunName
}

index! {
    /// A unique identifier for user-defined function
    UsrFunId
}

/// Function information
pub struct Function {
    /// a map from variable id to variables
    vars: BTreeMap<VarId, Variable>,
    /// a map from expression id to expressions
    exps: BTreeMap<ExpId, Expression>,
    /// parameters
    params: Vec<VarId>,
    /// the function body
    body: ExpId,
}

/// A registry of functions involved
pub struct FunRegistry {
    /// a map from user-defined functions and instantiations to function id
    sigs: BTreeMap<UsrFunName, BTreeMap<Vec<Sort>, UsrFunId>>,
    /// the actual function definitions
    defs: BTreeMap<UsrFunId, Function>,
}

impl FunRegistry {
    /// Initialize an empty registry
    pub fn new() -> Self {
        Self {
            sigs: BTreeMap::new(),
            defs: BTreeMap::new(),
        }
    }

    /// Get the index given a name and instantiation
    fn get_index(&self, name: &UsrFunName, inst: &[Sort]) -> Option<UsrFunId> {
        self.sigs.get(name)?.get(inst).copied()
    }

    /// Register a signature to the registry
    fn register_sig(&mut self, name: UsrFunName, inst: Vec<Sort>) -> UsrFunId {
        let idx = UsrFunId {
            index: self.sigs.values().map(|v| v.len()).sum::<usize>(),
        };
        let existing = self.sigs.entry(name).or_default().insert(inst, idx);
        if existing.is_some() {
            panic!("function signature already registered");
        }
        idx
    }

    /// Register a definition to the registry
    fn register_def(&mut self, idx: UsrFunId, def: Function) {
        let existing = self.defs.insert(idx, def);
        if existing.is_some() {
            panic!("function definition already registered");
        }
    }
}

impl<'a, 'ctx: 'a> IRBuilder<'a, 'ctx> {
    /// Resolve the function
    pub fn resolve_func(&mut self, fn_name: &UsrFuncName, ty_args: &[TypeRef]) -> Result<UsrFunId> {
        // derive the signature
        let name = UsrFunName {
            ident: fn_name.to_string(),
        };
        let ty_args = self.resolve_type_ref_vec(ty_args)?;

        // check if we have already processed the impl function
        match self.ir.fn_registry.get_index(&name, &ty_args) {
            None => (),
            Some(idx) => return Ok(idx),
        }

        // register the signature and get the index
        let idx = self.ir.fn_registry.register_sig(name, ty_args.clone());

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
        let mut builder = self.derive(generics, ty_args)?;

        // resolve type in function signatures
        for (param_name, param_ty) in params {
            builder.resolve_type(&(param_ty.into()))?;
        }
        builder.resolve_type(&(ret_ty.into()))?;

        // resolve body
        match body {
            None => todo!("axiom"),
            Some(exp) => {}
        }

        // done
        Ok(idx)
    }
}
