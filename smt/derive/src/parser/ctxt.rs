use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use log::trace;
use syn::{Attribute, Ident, Item, ItemEnum, ItemFn, ItemStruct, Meta, Result};
use walkdir::WalkDir;

use crate::parser::err::{bail_on, bail_on_with_note};
use crate::parser::name::{FuncName, TypeName};

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

/// Context manager for holding marked items
pub struct Context {
    types: BTreeMap<TypeName, MarkedType>,
    impls: BTreeMap<FuncName, MarkedImpl>,
    specs: BTreeMap<FuncName, MarkedSpec>,
}

impl Context {
    /// Build a context for crate
    pub fn new(path_input: &Path) -> Result<Self> {
        let mut ctxt = Self {
            types: BTreeMap::new(),
            impls: BTreeMap::new(),
            specs: BTreeMap::new(),
        };

        // test whether the path is a file or a directory
        if path_input.is_file() {
            ctxt.process_file(path_input)?;
        } else {
            // scan over the code base
            for entry in WalkDir::new(path_input) {
                let entry =
                    entry.unwrap_or_else(|err| panic!("unable to walk the directory: {}", err));
                let path = entry.path();
                if path.extension().map_or(false, |ext| ext == "rs") {
                    ctxt.process_file(path)?;
                }
            }
        }

        // return a collection of items as derivation context
        Ok(ctxt)
    }

    /// Process a single input file
    fn process_file(&mut self, path: &Path) -> Result<()> {
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
                        self.add_type(MarkedType::Enum(syntax))?;
                    }
                }
                Item::Struct(syntax) => {
                    if MarkedType::has_mark(&syntax.attrs) {
                        self.add_type(MarkedType::Struct(syntax))?;
                    }
                }
                Item::Fn(syntax) => {
                    if MarkedImpl::has_mark(&syntax.attrs) {
                        if MarkedSpec::has_mark(&syntax.attrs) {
                            bail_on!(&syntax, "function cannot be both spec and impl");
                        }
                        self.add_impl(MarkedImpl(syntax))?;
                    } else if MarkedSpec::has_mark(&syntax.attrs) {
                        self.add_spec(MarkedSpec(syntax))?;
                    }
                }
                _ => (),
            }
        }

        // processing completed
        Ok(())
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
        trace!("type found: {}", name);
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
        trace!("impl found: {}", name);
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
                "duplicated spec name"
            );
        }
        trace!("spec found: {}", name);
        self.specs.insert(name, item);
        Ok(())
    }
}
