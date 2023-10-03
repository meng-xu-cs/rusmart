use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use log::trace;
use proc_macro2::Span;
use syn::spanned::Spanned;
use syn::{Attribute, Error, Ident, Item, ItemEnum, ItemFn, ItemStruct, Meta, Result};
use walkdir::WalkDir;

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

    /// Retrieve the span of this item
    fn span(&self) -> Span {
        match self {
            Self::Enum(item) => item.span(),
            Self::Struct(item) => item.span(),
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

    /// Retrieve the span of this item
    fn span(&self) -> Span {
        self.0.span()
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

    /// Retrieve the span of this item
    fn span(&self) -> Span {
        self.0.span()
    }
}

/// Context manager of the entire derivation process
pub struct Context {
    types: BTreeMap<Ident, MarkedType>,
    impls: BTreeMap<Ident, MarkedImpl>,
    specs: BTreeMap<Ident, MarkedSpec>,
}

impl Context {
    /// Add a type to the context
    fn add_type(&mut self, item: MarkedType) -> Result<()> {
        let name = item.name();
        if let Some(existing) = self.types.get(name) {
            let mut error = Error::new(item.span(), format!("duplicated type name: {}", name));
            error.combine(Error::new(existing.span(), "previously defined here"));
            return Err(error);
        }
        trace!("type added to context: {}", name);
        self.types.insert(name.clone(), item);
        Ok(())
    }

    /// Add a impl function to the context
    fn add_impl(&mut self, item: MarkedImpl) -> Result<()> {
        let name = item.name();
        if let Some(existing) = self.impls.get(name) {
            let mut error = Error::new(item.span(), format!("duplicated impl name: {}", name));
            error.combine(Error::new(existing.span(), "previously defined here"));
            return Err(error);
        }
        trace!("impl added to context: {}", name);
        self.impls.insert(name.clone(), item);
        Ok(())
    }

    /// Add a spec function to the context
    fn add_spec(&mut self, item: MarkedSpec) -> Result<()> {
        let name = item.name();
        if let Some(existing) = self.specs.get(name) {
            let mut error = Error::new(item.span(), format!("duplicated spec name: {}", name));
            error.combine(Error::new(existing.span(), "previously defined here"));
            return Err(error);
        }
        trace!("spec added to context: {}", name);
        self.specs.insert(name.clone(), item);
        Ok(())
    }

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
                                    return Err(Error::new(
                                        syntax.span(),
                                        "function cannot be both spec and impl",
                                    ));
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
}
