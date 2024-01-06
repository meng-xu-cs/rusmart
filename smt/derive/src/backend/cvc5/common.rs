use std::path::PathBuf;

use lazy_static::lazy_static;

use rusmart_cli::cli::expect_cvc5;

use crate::backend::codegen::CodeGen;
use crate::backend::error::BackendResult;
use crate::ir::ctxt::IRContext;

lazy_static! {
    static ref ARTIFACT: PathBuf = expect_cvc5();
}

/// A generic backend for CVC5-related
pub trait BackendCVC5 {
    /// Mark the name of the backend
    fn name(&self) -> String;

    /// Produce the backend-specific code for the SMT model
    fn process(&self, ir: &IRContext) -> BackendResult<String>;
}

/// Wrapper for the backend
pub struct CodeGenCVC5<T: BackendCVC5> {
    backend: T,
}

impl<T: BackendCVC5> CodeGenCVC5<T> {
    pub fn new(backend: T) -> Self {
        Self { backend }
    }
}

impl<T: BackendCVC5> CodeGen for CodeGenCVC5<T> {
    fn name(&self) -> String {
        self.backend.name()
    }

    fn cmake(&self) -> String {
        include_str!("template.cmake")
            .replace("{:NAME:}", &self.name())
            .replace("{:ARTIFACT:}", ARTIFACT.to_str().expect("ascii path"))
    }

    fn flavor(&self) -> &'static str {
        "cpp"
    }

    fn process(&self, ir: &IRContext) -> BackendResult<String> {
        self.backend.process(ir)
    }
}
