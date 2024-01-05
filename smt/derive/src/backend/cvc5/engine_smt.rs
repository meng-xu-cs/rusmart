use crate::backend::codegen::{l, ContentBuilder};
use crate::backend::cvc5::common::BackendCVC5;
use crate::backend::error::BackendResult;
use crate::backend::exec::Response;
use crate::ir::ctxt::IRContext;

/// CHC engine
pub struct BackendCVC5SMT {}

impl BackendCVC5SMT {
    pub fn new() -> Self {
        Self {}
    }
}

impl BackendCVC5 for BackendCVC5SMT {
    fn name(&self) -> String {
        "cvc5_smt".to_string()
    }

    fn process(&self, ir: &IRContext) -> BackendResult<String> {
        let mut x = ContentBuilder::new();

        // includes
        l!(x, "#include <stdio.h>");
        l!(x, "#include <cvc5/cvc5.h>");
        l!(x);

        // main function
        l!(x, "// modeling for relation: {}", ir.desc);
        l!(x, "int main() {");
        x.scope(|x| {
            // TODO: content
            l!(x, "printf(\"{}\");", Response::Unknown);

            l!(x, "return 0;");
        });
        l!(x, "}");

        // done
        Ok(x.build())
    }
}
