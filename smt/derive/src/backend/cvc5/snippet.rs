use crate::backend::codegen::{l, ContentBuilder};

/// Variable of the context manager
const CTX: &str = "ctx";

pub struct Snippet {}

impl Snippet {
    /// Code for setup
    pub fn prologue(x: &mut ContentBuilder) {
        l!(x, "// prologue");
        l!(x, "Solver {};", CTX);
        l!(x);
    }
}
