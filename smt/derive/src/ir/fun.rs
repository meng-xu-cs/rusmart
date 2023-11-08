use std::collections::BTreeMap;

use crate::ir::exp::{ExpId, Expression, VarId, Variable};
use crate::ir::name::{index, name};
use crate::ir::sort::Sort;
use crate::parser::ctxt::ContextWithFunc;
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
}

/// Summarizes the task of analyzing one function
pub struct FunAnalysisTask {
    id: UsrFunId,
    name: UsrFuncName,
    inst: Vec<Sort>,
}

/// A contextualized holder for the type registry
pub struct FunRegistryHolder<'a, 'ctx: 'a> {
    /// information provider
    ctxt: &'ctx ContextWithFunc,
    /// a work list of functions to be analyzed
    worklist: Vec<FunAnalysisTask>,
    /// function registry to be modified
    registry: &'a mut FunRegistry,
}
