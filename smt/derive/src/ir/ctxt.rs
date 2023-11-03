use crate::parser::ctxt::{ContextWithFunc, Refinement};

/// A context manager for building around a refinement relation
pub struct IRBuilder;

impl IRBuilder {
    /// Initialize it with a new refinement relation
    pub fn build(ctxt: &ContextWithFunc, rel: &Refinement) {
        let (fn_impl, fn_spec) = ctxt.get_relation(rel);

        // initialize uninterpreted sorts
        let generics_impl = &fn_impl.head.generics.params;
        let generics_spec = &fn_spec.head.generics.params;
        assert!(generics_impl == generics_spec);
    }
}
