use std::fs;
use std::path::Path;

use crate::backend::codegen::{BackendResult, CodeGen};
use crate::ir::ctxt::IRContext;

/// A generic backend for Z3-related
pub trait BackendZ3 {
    /// Mark the name of the backend
    fn name(&self) -> String;

    /// Produce the backend-specific code for the SMT model
    fn process(&self, ir: &IRContext) -> BackendResult<String>;
}

/// Wrapper for the backend
pub struct CodeGenZ3<T: BackendZ3> {
    backend: T,
}

impl<T: BackendZ3> CodeGen for CodeGenZ3<T> {
    fn name(&self) -> String {
        self.backend.name()
    }

    fn execute(&self, ir: &IRContext, path_wks: &Path) -> BackendResult<()> {
        // generate
        let code = self.backend.process(ir)?;

        let path_main = path_wks.join("main.c");
        if path_main.exists() {
            panic!("main.c already exists");
        }
        fs::write(path_main, code)
            .unwrap_or_else(|e| panic!("IO error on writing to main.c: {}", e));

        // compile
        todo!()
    }
}
