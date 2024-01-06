use crate::backend::error::BackendResult;
use crate::ir::ctxt::IRContext;

/// Generic trait for backend code generator
pub trait CodeGen {
    /// Mark the name of the code generator
    fn name(&self) -> String;

    /// Content of the cmake file
    fn cmake(&self) -> String;

    /// Extension of the source code file
    fn flavor(&self) -> &'static str;

    /// Generate code for the IR context
    fn process(&self, ir: &IRContext) -> BackendResult<String>;
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

macro_rules! l {
    ($builder:expr) => {
        $builder.line("")
    };
    ($builder:expr, $item:expr) => {
        $builder.line($item)
    };
    ($builder:expr, $fmt:expr, $($args:tt)*) => {
        $builder.line(format!($fmt, $($args)*))
    };
}
pub(crate) use l;
