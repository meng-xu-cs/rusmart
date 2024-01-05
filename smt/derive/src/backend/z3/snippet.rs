use crate::backend::codegen::{l, ContentBuilder};

/// Variable of the config holder
const CFG: &str = "cfg";

/// Variable of the context manager
const CTX: &str = "ctx";

pub struct Snippet {}

impl Snippet {
    /// Code for setup
    pub fn prologue(x: &mut ContentBuilder) {
        l!(x, "// prologue");
        l!(x, "Z3_config {} = Z3_mk_config();", CFG);
        l!(x, "Z3_context {} = Z3_mk_context({});", CTX, CFG);
        l!(x, "Z3_del_config({});", CFG);
        l!(x);
    }

    /// Code for tear-down
    pub fn epilogue(x: &mut ContentBuilder) {
        l!(x);
        l!(x, "// epilogue");
        l!(x, "Z3_del_context({});", CTX);
    }
}
