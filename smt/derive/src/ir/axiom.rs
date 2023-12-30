use std::collections::BTreeMap;

use crate::ir::exp::{ExpId, ExpRegistry, Symbol};
use crate::ir::name::{index, name};
use crate::ir::sort::Sort;

name! {
    /// Name of an axiom
    UsrAxiomName
}

index! {
    /// A unique identifier for an axiom
    UsrAxiomId
}

/// Axiom
pub struct Axiom {
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
    axioms: BTreeMap<UsrAxiomId, Axiom>,
}

impl AxiomRegistry {
    /// Initialize an empty registry
    pub fn new() -> Self {
        Self {
            lookup: BTreeMap::new(),
            axioms: BTreeMap::new(),
        }
    }
}
