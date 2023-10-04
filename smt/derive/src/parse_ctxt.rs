use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::fs;
use std::path::Path;

use log::trace;
use syn::{Attribute, Error, Ident, Item, ItemEnum, ItemFn, ItemStruct, Meta, Result};
use walkdir::WalkDir;

use crate::parse_type::TypeDef;

/// Exit the parsing early with an error
macro_rules! bail_on {
    ($item:expr, $msg:literal $(,)?) => {
        return Err(syn::Error::new(syn::spanned::Spanned::span($item), $msg))
    };
    ($item:expr, $fmt:expr, $($arg:tt)*) => {
        return Err(syn::Error::new(syn::spanned::Spanned::span($item), format!($fmt, $($arg)*)))
    };
}
pub(crate) use bail_on;

/// Exit the parsing early with an error and a note
macro_rules! bail_on_with_note {
    ($loc:expr, $note:literal, $item:expr, $msg:literal $(,)?) => {
        return Err({
            let mut __e = syn::Error::new(syn::spanned::Spanned::span($item), $msg);
            __e.combine(syn::Error::new(syn::spanned::Spanned::span($loc), $note));
            __e
        })
    };
    ($loc:expr, $note:literal, $item:expr, $fmt:expr, $($arg:tt)*) => {
        return Err({
            let mut __e = syn::Error::new(syn::spanned::Spanned::span($item), format!($fmt, $($arg)*));
            __e.combine(syn::Error::new(syn::spanned::Spanned::span($loc), $note));
            __e
        })
    };
}
pub(crate) use bail_on_with_note;

/// Special case on bail: does not expect a token to exist
macro_rules! bail_if_exists {
    ($item:expr) => {
        match $item {
            None => (),
            Some(__v) => bail_on!(__v, "not expected"),
        }
    };
}
pub(crate) use bail_if_exists;

/// Special case on bail: does not expect a token to exist
macro_rules! bail_if_missing {
    ($item:expr, $par:expr, $note:literal) => {
        match $item {
            None => bail_on!($par, "expect {}", $note),
            Some(__v) => __v,
        }
    };
}
pub(crate) use bail_if_missing;

/// Identifier for a user-defined type (i.e., non-reserved)
#[derive(Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct TypeName {
    ident: String,
}

impl TryFrom<&Ident> for TypeName {
    type Error = Error;

    fn try_from(value: &Ident) -> Result<Self> {
        let name = value.to_string();
        match name.as_str() {
            "Boolean" | "Integer" | "Rational" | "Text" | "Box" | "Seq" | "Set" | "Map"
            | "Error" => bail_on!(value, "reserved type name"),
            _ => Ok(Self { ident: name }),
        }
    }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

/// Identifier for a user-defined function (i.e., non-reserved)
#[derive(Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct FuncName {
    ident: String,
}

impl Display for FuncName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl TryFrom<&Ident> for FuncName {
    type Error = Error;

    fn try_from(value: &Ident) -> Result<Self> {
        let name = value.to_string();
        match name.as_str() {
            // TODO: any reserved names?
            "_" => bail_on!(value, "reserved function name"),
            _ => Ok(Self { ident: name }),
        }
    }
}

/// SMT-marked type
pub enum MarkedType {
    Enum(ItemEnum),
    Struct(ItemStruct),
}

impl MarkedType {
    /// Test whether the SMT mark exists in the attributes
    fn has_mark(attrs: &[Attribute]) -> bool {
        attrs
            .iter()
            .any(|a| matches!(&a.meta, Meta::Path(name) if name.is_ident("smt_type")))
    }

    /// Retrieve the name of this item
    fn name(&self) -> &Ident {
        match self {
            Self::Enum(item) => &item.ident,
            Self::Struct(item) => &item.ident,
        }
    }
}

/// SMT-marked function as impl
pub struct MarkedImpl(ItemFn);

impl MarkedImpl {
    /// Test whether the SMT mark exists in the attributes
    fn has_mark(attrs: &[Attribute]) -> bool {
        attrs
            .iter()
            .any(|a| matches!(&a.meta, Meta::Path(name) if name.is_ident("smt_impl")))
    }

    /// Retrieve the name of this item
    fn name(&self) -> &Ident {
        &self.0.sig.ident
    }
}

/// SMT-marked function as spec
pub struct MarkedSpec(ItemFn);

impl MarkedSpec {
    /// Test whether the SMT mark exists in the attributes
    fn has_mark(attrs: &[Attribute]) -> bool {
        attrs
            .iter()
            .any(|a| matches!(&a.meta, Meta::List(targets) if targets.path.is_ident("smt_spec")))
    }

    /// Retrieve the name of this item
    fn name(&self) -> &Ident {
        &self.0.sig.ident
    }
}

/// Context manager of the entire derivation process
pub struct Context {
    types: BTreeMap<TypeName, MarkedType>,
    impls: BTreeMap<FuncName, MarkedImpl>,
    specs: BTreeMap<FuncName, MarkedSpec>,
}

impl Context {
    /// Build a context for crate
    pub fn new(path_crate: &Path) -> Result<Self> {
        let mut ctxt = Self {
            types: BTreeMap::new(),
            impls: BTreeMap::new(),
            specs: BTreeMap::new(),
        };

        // scan over the code base
        for entry in WalkDir::new(path_crate) {
            let entry = entry.unwrap_or_else(|err| panic!("unable to walk the directory: {}", err));
            let path = entry.path();
            if path.extension().map_or(false, |ext| ext == "rs") {
                // load and parse the source
                let content = fs::read_to_string(path).unwrap_or_else(|err| {
                    panic!(
                        "unable to read source file {}: {}",
                        path.to_string_lossy(),
                        err
                    )
                });
                let file = syn::parse_file(&content)?;

                // identify items marked with smt-related attributes
                for item in file.items {
                    match item {
                        Item::Enum(syntax) => {
                            if MarkedType::has_mark(&syntax.attrs) {
                                ctxt.add_type(MarkedType::Enum(syntax))?;
                            }
                        }
                        Item::Struct(syntax) => {
                            if MarkedType::has_mark(&syntax.attrs) {
                                ctxt.add_type(MarkedType::Struct(syntax))?;
                            }
                        }
                        Item::Fn(syntax) => {
                            if MarkedImpl::has_mark(&syntax.attrs) {
                                if MarkedSpec::has_mark(&syntax.attrs) {
                                    bail_on!(&syntax, "function cannot be both spec and impl");
                                }
                                ctxt.add_impl(MarkedImpl(syntax))?;
                            } else if MarkedSpec::has_mark(&syntax.attrs) {
                                ctxt.add_spec(MarkedSpec(syntax))?;
                            }
                        }
                        _ => (),
                    }
                }
            }
        }

        // return a collection of items as derivation context
        Ok(ctxt)
    }

    /// Add a type to the context
    fn add_type(&mut self, item: MarkedType) -> Result<()> {
        let name = item.name().try_into()?;
        if let Some(prev) = self.types.get(&name) {
            bail_on_with_note!(
                prev.name(),
                "previously defined here",
                item.name(),
                "duplicated type name"
            );
        }
        trace!("type added to context: {}", name);
        self.types.insert(name, item);
        Ok(())
    }

    /// Add a impl function to the context
    fn add_impl(&mut self, item: MarkedImpl) -> Result<()> {
        let name = item.name().try_into()?;
        if let Some(prev) = self.impls.get(&name) {
            bail_on_with_note!(
                prev.name(),
                "previously defined here",
                item.name(),
                "duplicated impl name"
            );
        }
        trace!("impl added to context: {}", name);
        self.impls.insert(name, item);
        Ok(())
    }

    /// Add a spec function to the context
    fn add_spec(&mut self, item: MarkedSpec) -> Result<()> {
        let name = item.name().try_into()?;
        if let Some(prev) = self.specs.get(&name) {
            bail_on_with_note!(
                prev.name(),
                "previously defined here",
                item.name(),
                "duplicated impl name"
            );
        }
        trace!("spec added to context: {}", name);
        self.specs.insert(name, item);
        Ok(())
    }

    /// Retrieve type if exists
    pub fn get_type(&self, name: &TypeName) -> Option<&MarkedType> {
        self.types.get(name)
    }

    /// Parse types
    pub fn analyze(self) -> Result<ContextWithType> {
        let mut parsed_types = BTreeMap::new();
        for (name, marked) in &self.types {
            let parsed = TypeDef::from_marked(&self, marked)?;
            trace!("type analyzed: {}", name);
            parsed_types.insert(name.clone(), parsed);
        }

        let Self {
            types: _,
            impls,
            specs,
        } = self;
        Ok(ContextWithType {
            types: parsed_types,
            impls,
            specs,
        })
    }
}

/// Context manager of the entire derivation process
pub struct ContextWithType {
    types: BTreeMap<TypeName, TypeDef>,
    impls: BTreeMap<FuncName, MarkedImpl>,
    specs: BTreeMap<FuncName, MarkedSpec>,
}

impl ContextWithType {
    /// Retrieve type if exists
    pub fn get_type(&self, name: &TypeName) -> Option<&TypeDef> {
        self.types.get(name)
    }

    /// Retrieve impl function if exists
    pub fn get_impl(&self, name: &FuncName) -> Option<&MarkedImpl> {
        self.impls.get(name)
    }

    /// Retrieve spec function if exists
    pub fn get_spec(&self, name: &FuncName) -> Option<&MarkedSpec> {
        self.specs.get(name)
    }
}
