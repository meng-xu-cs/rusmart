use crate::analysis::sort::sort_in_topological_order;
use crate::backend::codegen::{l, ContentBuilder};
use crate::backend::error::BackendResult;
use crate::backend::exec::Response;
use crate::backend::z3::common::BackendZ3;
use crate::backend::z3::snippet::Snippet;
use crate::ir::ctxt::IRContext;

/// CHC engine
pub struct BackendZ3CHC {}

impl BackendZ3CHC {
    pub fn new() -> Self {
        Self {}
    }
}

impl BackendZ3 for BackendZ3CHC {
    fn name(&self) -> String {
        "z3_chc".to_string()
    }

    fn process(&self, ir: &IRContext) -> BackendResult<String> {
        let mut x = ContentBuilder::new();

        // includes
        l!(x, "#include <stdio.h>");
        l!(x, "#include <z3.h>");
        l!(x);

        // main function
        l!(x, "// modeling for relation: {}", ir.desc);
        l!(x, "int main() {");
        x.scope(|x| {
            Snippet::prologue(x);

            // define uninterpreted sorts
            for sort_name in &ir.undef_sorts {
                Snippet::def_uninterpreted_sort(x, sort_name);
            }

            // define user-defined datatypes
            for scc in sort_in_topological_order(&ir.ty_registry) {}

            // TODO: content
            l!(x, "printf(\"{}\");", Response::Unknown);

            Snippet::epilogue(x);
            l!(x, "return 0;");
        });
        l!(x, "}");

        // done
        Ok(x.build())
    }
}
