use crate::backend::codegen::{l, ContentBuilder};
use crate::ir::name::SmtSortName;

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

    /// Refer to an uninterpreted sort
    fn ref_uninterpreted_sort(sort: &SmtSortName) -> String {
        format!("sort_{}", sort)
    }

    /// Define an uninterpreted sort
    pub fn def_uninterpreted_sort(x: &mut ContentBuilder, sort: &SmtSortName) {
        l!(
            x,
            "Z3_sort {} = Z3_mk_uninterpreted_sort({}, Z3_mk_string_symbol({}, \"{}\"));",
            Self::ref_uninterpreted_sort(sort),
            CTX,
            CTX,
            sort,
        )
    }
}
