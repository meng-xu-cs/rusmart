use crate::backend::codegen::ContentBuilder;
use crate::backend::error::BackendResult;
use crate::backend::exec::Response;
use crate::backend::z3::common::BackendZ3;
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
        x.line("#include <stdio.h>");
        x.line("#include <z3.h>");

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
