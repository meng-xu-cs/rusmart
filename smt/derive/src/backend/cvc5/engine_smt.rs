use crate::backend::codegen::ContentBuilder;
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
        x.line("#include <stdio.h>");
        x.line("#include <cvc5/cvc5.h>");

        // main function
        x.line("int main() {");
        x.scope(|x| {
            x.line(format!("// modeling for relation: {}", ir.desc));

            // TODO: content
            x.line(format!("printf(\"{}\");", Response::Unknown));

            x.line("return 0;");
        });
        x.line("}");

        // done
        Ok(x.build())
    }
}
