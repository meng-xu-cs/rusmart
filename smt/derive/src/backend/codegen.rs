use std::path::Path;

use crate::backend::error::BackendResult;
use crate::ir::ctxt::IRContext;

/// Generic trait for backend code generator
pub trait CodeGen {
    /// Mark the name of the code generator
    fn name(&self) -> String;

    /// Execute the IR with backend-specific logic
    fn execute(&self, ir: &IRContext, path_wks: &Path) -> BackendResult<()>;
}

/// A utility for source code builder
pub struct ContentBuilder {
    buffer: String,
    indent: usize,
}

impl ContentBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
            indent: 0,
        }
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
    pub fn scope<F: Fn(&mut Self)>(&mut self, f: F) {
        self.indent += 1;
        f(self);
        self.indent -= 1;
    }

    /// Convert it to string
    pub fn build(self) -> String {
        self.buffer
    }
}
