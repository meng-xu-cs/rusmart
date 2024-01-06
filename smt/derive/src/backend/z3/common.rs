use std::path::PathBuf;

use lazy_static::lazy_static;

use rusmart_cli::cli::expect_z3;

use crate::backend::codegen::CodeGen;
use crate::backend::error::BackendResult;
use crate::ir::ctxt::IRContext;

lazy_static! {
    static ref ARTIFACT: PathBuf = expect_z3();
}

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

impl<T: BackendZ3> CodeGenZ3<T> {
    pub fn new(backend: T) -> Self {
        Self { backend }
    }
}

impl<T: BackendZ3> CodeGen for CodeGenZ3<T> {
    fn name(&self) -> String {
        self.backend.name()
    }

    fn cmake(&self) -> String {
        include_str!("template.cmake")
            .replace("{:NAME:}", &self.name())
            .replace("{:ARTIFACT:}", ARTIFACT.to_str().expect("ascii path"))
    }

    fn flavor(&self) -> &'static str {
        "c"
    }

    fn process(&self, ir: &IRContext) -> BackendResult<String> {
        self.backend.process(ir)
    }
}
