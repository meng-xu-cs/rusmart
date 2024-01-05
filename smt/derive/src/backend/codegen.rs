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

/// A utility for source code builder
pub struct ContentBuilder<'a> {
    buffer: &'a mut String,
    indent: usize,
}

impl<'a> ContentBuilder<'a> {
    /// Create a new builder
    pub fn new(buffer: &'a mut String) -> Self {
        Self { buffer, indent: 0 }
    }

    /// Add a new line to the content
    pub fn line<S: AsRef<str>>(&mut self, code: S) {
        for _ in 0..self.indent {
            self.buffer.push('\t');
        }
        self.buffer.push_str(code.as_ref());
        self.buffer.push('\n');
    }

    /// Indented builder
    pub fn indent(&'a mut self) -> Self {
        Self {
            buffer: self.buffer,
            indent: self.indent + 1,
        }
    }
}
