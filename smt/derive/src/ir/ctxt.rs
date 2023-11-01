use crate::parser::ctxt::{ContextWithFunc, Refinement};

/// A context manager for building around a refinement relation
pub struct IRBuilder;

impl IRBuilder {
    /// Initialize it with a new refinement relation
    pub fn build(ctxt: &ContextWithFunc, rel: &Refinement) {
        let (fn_impl, fn_spec) = ctxt.get_relation(rel);
    }
}
