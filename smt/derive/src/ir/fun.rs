use std::collections::BTreeMap;

use crate::ir::exp::{ExpId, Expression, VarId, Variable};
use crate::ir::name::{index, name};
use crate::ir::sort::Sort;

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
    /// a map from user-defined functions and instantiations to node indices
    idx_named: BTreeMap<UsrFunName, BTreeMap<Vec<Sort>, UsrFunId>>,
    /// the actual function definitions
    defs: BTreeMap<UsrFunId, Function>,
}
