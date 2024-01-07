use std::collections::{BTreeMap, BTreeSet};

use crate::analysis::sort::probe_optionals_for_datatype;
use crate::backend::codegen::{l, ContentBuilder};
use crate::ir::index::UsrSortId;
use crate::ir::name::SmtSortName;
use crate::ir::sort::{DataType, Sort};

/// Variable of the config holder
const CFG: &str = "cfg";

/// Variable of the context manager
const CTX: &str = "ctx";

/// Bitsize for the error type
const ERROR_BV_SIZE: usize = 1024;

/// Code accumulation session
pub struct Session {
    /// symbol count
    symbol_count: usize,
    /// naming map for uninterpreted sorts
    sorts_uninterpreted: BTreeMap<SmtSortName, String>,
    /// naming map for user-defined data type sorts
    sorts_data_type: BTreeMap<UsrSortId, String>,
    /// naming map for optional sorts
    sorts_optional: BTreeMap<Sort, String>,
}

impl Session {
    /// Code for setup
    pub fn prologue(x: &mut ContentBuilder) -> Self {
        l!(x, "// prologue");
        l!(x, "Z3_config {} = Z3_mk_config();", CFG);
        l!(x, "Z3_context {} = Z3_mk_context({});", CTX, CFG);
        l!(x, "Z3_del_config({});", CFG);
        l!(x);

        // initialize the states
        Self {
            symbol_count: 0,
            sorts_uninterpreted: BTreeMap::new(),
            sorts_data_type: BTreeMap::new(),
            sorts_optional: BTreeMap::new(),
        }
    }

    /// Code for tear-down
    pub fn epilogue(self, x: &mut ContentBuilder) {
        l!(x);
        l!(x, "// epilogue");
        l!(x, "Z3_del_context({});", CTX);
    }

    /// Create a new symbol
    fn new_symbol(&mut self) -> String {
        self.symbol_count += 1;
        format!("Z3_mk_int_symbol({}, {})", CTX, self.symbol_count)
    }

    /// Create a string symbol
    fn str_symbol(name: &str) -> String {
        format!("Z3_mk_string_symbol({}, \"{}\")", CTX, name)
    }

    /// Refer to an uninterpreted sort
    fn ref_uninterpreted_sort(&self, name: &SmtSortName) -> &str {
        self.sorts_uninterpreted
            .get(name)
            .unwrap_or_else(|| panic!("uninterpreted sort not declared: {}", name))
    }

    /// Define an uninterpreted sort
    pub fn def_uninterpreted_sort(&mut self, x: &mut ContentBuilder, name: &SmtSortName) {
        let var = format!("sort_uninterpreted_{}", name);
        l!(
            x,
            "Z3_sort {} = Z3_mk_uninterpreted_sort({}, {});",
            var,
            CTX,
            self.new_symbol(),
        );

        if self.sorts_uninterpreted.insert(name.clone(), var).is_some() {
            panic!("duplicated definition of uninterpreted sort: {}", name);
        }
    }

    /// Refer to a sort
    fn ref_sort(&self, sort: &Sort) -> String {
        match sort {
            Sort::Boolean => format!("Z3_mk_bool_sort({})", CTX),
            Sort::Integer => format!("Z3_mk_int_sort({})", CTX),
            Sort::Rational => format!("Z3_mk_real_sort({})", CTX),
            Sort::Text => format!("Z3_mk_string_sort({})", CTX),
            Sort::Seq(sub) => format!("Z3_mk_seq_sort({}, {})", CTX, self.ref_sort(sub)),
            Sort::Set(sub) => format!("Z3_mk_set_sort({}, {})", CTX, self.ref_sort(sub)),
            Sort::Map(key, val) => format!(
                "Z3_mk_array_sort({}, {}, {})",
                CTX,
                self.ref_sort(key),
                self.ref_optional_sort(val),
            ),
            Sort::Error => format!("Z3_mk_bv_sort({}, {})", CTX, ERROR_BV_SIZE),
            Sort::User(sid) => self.ref_datatype_sort(*sid).to_string(),
            Sort::Uninterpreted(name) => self.ref_uninterpreted_sort(name).to_string(),
        }
    }

    /// Refer to an optional<T> data type
    fn ref_optional_sort(&self, sort: &Sort) -> &str {
        self.sorts_optional
            .get(sort)
            .unwrap_or_else(|| panic!("optional<sort> not declared: {}", sort))
    }

    /// Define an optional<sort> based on sort
    fn def_optional_sort(&mut self, x: &mut ContentBuilder, sort: &Sort) {
        let var = format!("sort_optional_{}", sort);

        // make constructors
        let ctor_none = format!("ctor_{}_none", var);
        l!(
            x,
            "Z3_constructor {} = Z3_mk_constructor({}, {}, {}, 0, (Z3_symbol[]){{}}, (Z3_sort_opt[]){{}}, (unsigned[]){{}})",
            ctor_none,
            CTX,
            Self::str_symbol("None"),
            Self::str_symbol("is_none")
        );

        let ctor_some = format!("ctor_{}_some", var);
        l!(
            x,
            "Z3_constructor {} = Z3_mk_constructor({}, {}, {}, 1, (Z3_symbol[]){{{}}}, (Z3_sort_opt[]){{{}}}, (unsigned[]){{}})",
            ctor_some,
            CTX,
            Self::str_symbol("Some"),
            Self::str_symbol("is_some"),
            Self::str_symbol("some"),
            self.ref_sort(sort),
        );

        // make datatype
        l!(
            x,
            "Z3_sort {} = Z3_mk_datatype({}, {}, 2, (Z3_constructor[]){{{}, {}}});",
            var,
            CTX,
            self.new_symbol(),
            ctor_none,
            ctor_some,
        );

        if self.sorts_optional.insert(sort.clone(), var).is_some() {
            panic!("duplicated definition of optional<sort>: {}", sort);
        }
    }

    /// Refer to a user-defined data type
    fn ref_datatype_sort(&self, sid: UsrSortId) -> &str {
        self.sorts_data_type
            .get(&sid)
            .unwrap_or_else(|| panic!("datatype sort not declared: {}", sid))
    }

    /// Define a user-defined data type
    pub fn def_datatype_single(&mut self, x: &mut ContentBuilder, sid: UsrSortId, dt: &DataType) {
        // probe and define (if not yet defined) optional sorts
        let mut optionals = BTreeSet::new();
        probe_optionals_for_datatype(dt, &mut optionals);
        for sort in optionals {
            if self.sorts_optional.contains_key(&sort) {
                continue;
            }
            self.def_optional_sort(x, &sort);
        }

        match dt {
            DataType::Tuple(slots) => todo!(),
            DataType::Record(fields) => todo!(),
            DataType::Enum(variants) => todo!(),
        }
    }
}
