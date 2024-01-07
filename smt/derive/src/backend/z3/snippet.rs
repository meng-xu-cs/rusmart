use crate::backend::codegen::{l, ContentBuilder};
use crate::ir::index::UsrSortId;
use crate::ir::name::SmtSortName;
use crate::ir::sort::{DataType, Sort};

/// Variable of the config holder
const CFG: &str = "cfg";

/// Variable of the context manager
const CTX: &str = "ctx";

/// Error types bitsize
const ERROR_BV_SIZE: usize = 1024;

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
    fn ref_uninterpreted_sort(name: &SmtSortName) -> String {
        format!("sort_{}", name)
    }

    /// Define an uninterpreted sort
    pub fn def_uninterpreted_sort(x: &mut ContentBuilder, name: &SmtSortName) {
        l!(
            x,
            "Z3_sort {} = Z3_mk_uninterpreted_sort({}, Z3_mk_string_symbol({}, \"{}\"));",
            Self::ref_uninterpreted_sort(name),
            CTX,
            CTX,
            name,
        )
    }

    /// Refer to a user-defined data type
    fn ref_user_sort(sid: UsrSortId) -> String {
        format!("type_{}", sid)
    }

    /// Refer to a sort
    fn ref_sort(sort: &Sort) -> String {
        match sort {
            Sort::Boolean => format!("Z3_mk_bool_sort({})", CTX),
            Sort::Integer => format!("Z3_mk_int_sort({})", CTX),
            Sort::Rational => format!("Z3_mk_real_sort({})", CTX),
            Sort::Text => format!("Z3_mk_string_sort({})", CTX),
            Sort::Seq(sub) => format!("Z3_mk_seq_sort({}, {})", CTX, Snippet::ref_sort(sub)),
            Sort::Set(sub) => format!("Z3_mk_set_sort({}, {})", CTX, Snippet::ref_sort(sub)),
            Sort::Map(key, val) => {
                // process the option sort first
                format!("Z3_mk_array_sort({}, {})", CTX, Snippet::ref_sort(key));
                todo!()
            }
            Sort::Error => format!("Z3_mk_bv_sort({}, {})", CTX, ERROR_BV_SIZE),
            Sort::User(sid) => Self::ref_user_sort(*sid),
            Sort::Uninterpreted(name) => Self::ref_uninterpreted_sort(name),
        }
    }

    /// Define a user-defined data type
    pub fn def_datatype_single(x: &mut ContentBuilder, dt: &DataType) {
        match dt {
            DataType::Tuple(slots) => todo!(),
            DataType::Record(fields) => todo!(),
            DataType::Enum(variants) => todo!(),
        }
    }
}
