use crate::backend::codegen::{l, ContentBuilder};
use crate::ir::name::SmtSortName;

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

    /// Refer to an uninterpreted sort
    fn ref_uninterpreted_sort(sort: &SmtSortName) -> String {
        format!("sort_{}", sort)
    }

    /// Define an uninterpreted sort
    pub fn def_uninterpreted_sort(x: &mut ContentBuilder, sort: &SmtSortName) {
        l!(
            x,
            "Sort {} = {}.mkUninterpretedSort(\"{}\");",
            Self::ref_uninterpreted_sort(sort),
            CTX,
            sort,
        )
    }
}
