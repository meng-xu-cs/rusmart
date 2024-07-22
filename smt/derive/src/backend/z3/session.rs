use std::collections::{BTreeMap, BTreeSet};

use crate::analysis::sort::probe_optionals_for_datatype;
use crate::backend::codegen::{l, ContentBuilder};
use crate::ir::index::UsrSortId;
use crate::ir::name::SmtSortName;
use crate::ir::sort::{DataType, Sort, TypeRegistry};

/// Variable of the config holder
const CFG: &str = "cfg";

/// Variable of the context manager
const CTX: &str = "ctx";

/// Bitsize for the error type
const ERROR_BV_SIZE: usize = 1024;

/// Utility macro to define a variable name
macro_rules! var {
    ($(#[$meta:meta])* $name:ident) => {
        $(#[$meta])*
        #[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
        pub struct $name {
            ident: String,
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &str {
                &self.ident
            }
        }

        impl From<String> for $name {
            fn from(ident: String) -> Self {
                Self {ident}
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.ident)
            }
        }
    };
}

var! {
    /// Z3_sort
    Z3Sort
}

var! {
    /// Z3_func_decl
    Z3FuncDecl
}

var! {
    /// Z3_constructor
    Z3Ctor
}

/// Datatype pack for optional<sort>
struct PackOptional {
    sort_name: Z3Sort,
    mk_none: Z3FuncDecl,
    is_none: Z3FuncDecl,
    mk_some: Z3FuncDecl,
    is_some: Z3FuncDecl,
    get_some: Z3FuncDecl,
}

/// A variant for an enum
enum EnumBranch {
    Unit {
        ctor: Z3FuncDecl,
        tester: Z3FuncDecl,
    },
    Tuple {
        ctor: Z3FuncDecl,
        tester: Z3FuncDecl,
        getters: Vec<Z3FuncDecl>,
    },
    Record {
        ctor: Z3FuncDecl,
        tester: Z3FuncDecl,
        getters: BTreeMap<String, Z3FuncDecl>,
    },
}

/// Datatype details for user-defined algebraic data types
enum ADTDetails {
    Tuple {
        ctor: Z3FuncDecl,
        getters: Vec<Z3FuncDecl>,
    },
    Record {
        ctor: Z3FuncDecl,
        getters: BTreeMap<String, Z3FuncDecl>,
    },
    Enum {
        variants: BTreeMap<String, EnumBranch>,
    },
}

/// Datatype pack for user-defined algebraic data types
struct PackADT {
    sort_name: Z3Sort,
    details: ADTDetails,
}

/// Code accumulation session
pub struct Session {
    /// symbol count
    symbol_count: usize,
    /// naming map for uninterpreted sorts
    sorts_uninterpreted: BTreeMap<SmtSortName, Z3Sort>,
    /// naming map for user-defined algebraic data types
    sorts_adt: BTreeMap<UsrSortId, Z3Sort>,
    /// naming map for optional sorts
    sorts_optional: BTreeMap<Sort, PackOptional>,
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
            sorts_adt: BTreeMap::new(),
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

    /// Create an integer symbol
    fn int_symbol(index: usize) -> String {
        format!("Z3_mk_int_symbol({}, {})", CTX, index)
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
            .as_ref()
    }

    /// Define an uninterpreted sort
    pub fn def_uninterpreted_sort(&mut self, x: &mut ContentBuilder, name: &SmtSortName) {
        let var = Z3Sort::from(format!("sort_uninterpreted_{}", name));
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
            Sort::User(sid) => self.ref_adt_sort(*sid).to_string(),
            Sort::Uninterpreted(name) => self.ref_uninterpreted_sort(name).to_string(),
        }
    }

    /// Refer to an optional<T> data type
    fn ref_optional_sort(&self, sort: &Sort) -> &str {
        self.sorts_optional
            .get(sort)
            .unwrap_or_else(|| panic!("optional<sort> not declared: {}", sort))
            .sort_name
            .as_ref()
    }

    /// Define an optional<sort> based on sort
    fn def_optional_sort(&mut self, x: &mut ContentBuilder, sort: &Sort) {
        let var = Z3Sort::from(format!("sort_optional_{}", sort));

        // make constructors
        let ctor_none = Z3Ctor::from(format!("ctor_{}_none", var));
        l!(
            x,
            "Z3_constructor {} = Z3_mk_constructor({}, {}, {}, 0, (Z3_symbol[]){{}}, (Z3_sort_opt[]){{}}, (unsigned[]){{}})",
            ctor_none,
            CTX,
            Self::str_symbol("None"),
            Self::str_symbol("is_none")
        );

        let ctor_some = Z3Ctor::from(format!("ctor_{}_some", var));
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

        // retrieve accessors and testers
        let mk_none = Z3FuncDecl::from(format!("func_{}_mk_none", var));
        let is_none = Z3FuncDecl::from(format!("func_{}_is_none", var));
        l!(x, "Z3_func_decl {};", mk_none);
        l!(x, "Z3_func_decl {};", is_none);
        l!(
            x,
            "Z3_query_constructor({}, {}, 0, &{}, &{}, (Z3_func_decl[]){{}});",
            CTX,
            ctor_none,
            mk_none,
            is_none,
        );

        let mk_some = Z3FuncDecl::from(format!("func_{}_mk_some", var));
        let is_some = Z3FuncDecl::from(format!("func_{}_is_some", var));
        let get_some = Z3FuncDecl::from(format!("func_{}_get_some", var));
        l!(x, "Z3_func_decl {};", mk_some);
        l!(x, "Z3_func_decl {};", is_some);
        l!(x, "Z3_func_decl {};", get_some);
        l!(
            x,
            "Z3_query_constructor({}, {}, 1, &{}, &{}, (Z3_func_decl[]){{{}}});",
            CTX,
            ctor_some,
            mk_some,
            is_some,
            get_some
        );

        // register it in the states
        let pack = PackOptional {
            sort_name: var,
            mk_none,
            is_none,
            mk_some,
            is_some,
            get_some,
        };
        if self.sorts_optional.insert(sort.clone(), pack).is_some() {
            panic!("duplicated definition of optional<sort>: {}", sort);
        }
    }

    /// Refer to a user-defined data type
    fn ref_adt_sort(&self, sid: UsrSortId) -> &str {
        self.sorts_adt
            .get(&sid)
            .unwrap_or_else(|| panic!("datatype sort not declared: {}", sid))
            .as_ref()
    }

    /// Define one user-defined ADT
    pub fn def_adt_single(
        &mut self,
        x: &mut ContentBuilder,
        sid: UsrSortId,
        registry: &TypeRegistry,
    ) {
        // query the data type first
        let dt = registry.retrieve(sid);

        // probe and define (if not yet defined) optional sorts
        let mut optionals = BTreeSet::new();
        probe_optionals_for_datatype(dt, &mut optionals);
        for sort in optionals {
            if !self.sorts_optional.contains_key(&sort) {
                self.def_optional_sort(x, &sort);
            }
        }

        // define the algebraic data type (ADT)
        match dt {
            DataType::Tuple(_slots) => todo!(),
            DataType::Record(_fields) => todo!(),
            DataType::Enum(_variants) => todo!(),
        }
    }

    /// Define a user-defined mutually recursive ADT group
    pub fn def_adt_group(
        &mut self,
        x: &mut ContentBuilder,
        group: &BTreeSet<UsrSortId>,
        registry: &TypeRegistry,
    ) {
        // query the data type first
        let dts: BTreeMap<_, _> = group
            .iter()
            .map(|sid| (*sid, registry.retrieve(*sid)))
            .collect();

        // probe and define (if not yet defined) optional sorts
        for &dt in dts.values() {
            let mut optionals = BTreeSet::new();
            probe_optionals_for_datatype(dt, &mut optionals);
            // TODO: recheck
            for sort in optionals {
                if !self.sorts_optional.contains_key(&sort) {
                    self.def_optional_sort(x, &sort);
                }
            }
        }

        // define the recursive ADT group

        todo!()
    }
}
