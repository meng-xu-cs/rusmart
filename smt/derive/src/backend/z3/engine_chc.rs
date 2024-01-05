use crate::backend::codegen::{BackendResult, ContentBuilder};
use crate::backend::z3::common::BackendZ3;
use crate::ir::ctxt::IRContext;

/// CHC engine
pub struct BackendZ3CHC {}

impl BackendZ3 for BackendZ3CHC {
    fn name(&self) -> String {
        "z3_chc".to_string()
    }

    fn process(&self, ir: &IRContext) -> BackendResult<String> {
        let mut content = String::new();
        let mut builder = ContentBuilder::new(&mut content);

        // includes
        builder.line("#include <z3.h>");

        // main function
        builder.line("int main() {");
        {
            let mut f_main = builder.indent();
            f_main.line(format!("// modeling for relation: {}", ir.desc));
            // TODO: content
            f_main.line("return 0;");
        }
        builder.line("}");

        // done
        Ok(content)
    }
}
