use std::path::Path;

use crate::ir::ctxt::IRContext;

/// An error for backend generator
pub enum BackendError {
    NotSupported,
}

pub type BackendResult<T> = Result<T, BackendError>;

/// Generic trait for backend code generator
pub trait CodeGen {
    /// Mark the name of the code generator
    fn name(&self) -> String;

    /// Execute the IR with backend-specific logic
    fn execute(&self, ir: &IRContext, path_wks: &Path) -> BackendResult<()>;
}
