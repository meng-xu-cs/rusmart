use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use log::trace;
use syn::{Attribute, File, Ident, Item, ItemEnum, ItemFn, ItemStruct, Meta, Result, Stmt};
use walkdir::WalkDir;

use crate::parser::err::{bail_on, bail_on_with_note};
use crate::parser::func::{FuncDef, FuncSig};
use crate::parser::generics::Generics;
use crate::parser::name::{UsrFuncName, UsrTypeName};
use crate::parser::ty::{TypeBody, TypeDef};

use crate::parser::expr::{ExprParserRoot, Kind};
use crate::parser::infer::InferDatabase;

#[cfg(test)]
use proc_macro2::TokenStream;

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
    types: BTreeMap<UsrTypeName, MarkedType>,
    impls: BTreeMap<UsrFuncName, MarkedImpl>,
    specs: BTreeMap<UsrFuncName, MarkedSpec>,
}

impl Context {
    /// Build a context for crate
    pub fn new(path_input: &Path) -> Result<Self> {
        let mut ctxt = Self {
            types: BTreeMap::new(),
            impls: BTreeMap::new(),
            specs: BTreeMap::new(),
        };

        // scan over the code base
        for entry in WalkDir::new(path_input) {
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

                // process it
                ctxt.process_file(file)?;
            }
        }

        // return a collection of items as derivation context
        Ok(ctxt)
    }

    /// Process a single input file
    fn process_file(&mut self, file: File) -> Result<()> {
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

    /// Parse the generics declarations
    pub fn parse_generics(self) -> Result<ContextWithGenerics> {
        let mut types = BTreeMap::new();
        for (name, marked) in self.types {
            let parsed = Generics::from_marked_type(&marked)?;
            types.insert(name, (parsed, marked));
        }

        Ok(ContextWithGenerics {
            types,
            impls: self.impls,
            specs: self.specs,
        })
    }

    #[cfg(test)]
    pub fn new_from_stream(stream: TokenStream) -> Result<Self> {
        // init
        let mut ctxt = Self {
            types: BTreeMap::new(),
            impls: BTreeMap::new(),
            specs: BTreeMap::new(),
        };

        // parse
        let file: File = syn::parse2(stream)?;
        ctxt.process_file(file)?;
        Ok(ctxt)
    }
}

/// Context manager after analyzing for generics
pub struct ContextWithGenerics {
    types: BTreeMap<UsrTypeName, (Generics, MarkedType)>,
    impls: BTreeMap<UsrFuncName, MarkedImpl>,
    specs: BTreeMap<UsrFuncName, MarkedSpec>,
}

impl ContextWithGenerics {
    /// Get the generics declaration for a type
    pub fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics> {
        self.types.get(name).map(|(generics, _)| generics)
    }

    /// Parse types
    pub fn parse_types(self) -> Result<ContextWithType> {
        // parsing
        let mut parsed_types = BTreeMap::new();
        for (name, (generics, marked)) in &self.types {
            trace!("handling type: {}", name);
            let parsed = TypeBody::from_marked(&self, generics, marked)?;
            trace!("type analyzed: {}", name);
            parsed_types.insert(name.clone(), parsed);
        }

        // re-packing
        let Self {
            types,
            impls,
            specs,
        } = self;

        let new_types = types
            .into_iter()
            .map(|(name, (generics, _))| {
                let body = parsed_types.remove(&name).unwrap();
                (name, TypeDef::new(generics, body))
            })
            .collect();

        Ok(ContextWithType {
            types: new_types,
            impls,
            specs,
        })
    }
}

/// Context manager after type analysis is done
pub struct ContextWithType {
    types: BTreeMap<UsrTypeName, TypeDef>,
    impls: BTreeMap<UsrFuncName, MarkedImpl>,
    specs: BTreeMap<UsrFuncName, MarkedSpec>,
}

impl ContextWithType {
    /// Get the generics declaration for a type
    pub fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics> {
        self.types.get(name).map(|def| def.head())
    }

    /// Parse function signatures
    pub fn parse_func_sigs(self) -> Result<ContextWithSig> {
        // impl
        let mut sig_impls = BTreeMap::new();
        for (name, marked) in &self.impls {
            let ItemFn {
                attrs: _,
                vis: _,
                sig,
                block: _, // handled later
            } = &marked.0;

            trace!("handling impl sig: {}", name);
            let sig = FuncSig::from_sig(&self, sig)?;
            trace!("impl sig analyzed: {}", name);
            sig_impls.insert(name.clone(), sig);
        }

        // spec
        let mut sig_specs = BTreeMap::new();
        for (name, marked) in &self.specs {
            let ItemFn {
                attrs: _,
                vis: _,
                sig,
                block: _, // handled later
            } = &marked.0;

            trace!("handling spec sig: {}", name);
            let sig = FuncSig::from_sig(&self, sig)?;
            trace!("spec sig analyzed: {}", name);
            sig_specs.insert(name.clone(), sig);
        }

        // re-packing
        let Self {
            types,
            impls,
            specs,
        } = self;

        let unpacked_impls: BTreeMap<_, _> = impls
            .into_iter()
            .map(|(name, marked)| {
                let sig = sig_impls.remove(&name).unwrap();
                let stmts = marked.0.block.stmts;
                (name, (sig, stmts))
            })
            .collect();
        let unpacked_specs: BTreeMap<_, _> = specs
            .into_iter()
            .map(|(name, marked)| {
                let sig = sig_specs.remove(&name).unwrap();
                let stmts = marked.0.block.stmts;
                (name, (sig, stmts))
            })
            .collect();

        // populate the inference database
        let mut infer = InferDatabase::with_intrinsics();
        for (name, (sig, _)) in unpacked_impls.iter() {
            infer.register_user_func(name, sig);
        }

        trace!("function database populated with {} entries", infer.size());

        // done
        let ctxt = ContextWithSig {
            types,
            impls: unpacked_impls,
            specs: unpacked_specs,
            infer,
        };
        Ok(ctxt)
    }
}

/// Context manager after type and function signature analysis is done
pub struct ContextWithSig {
    types: BTreeMap<UsrTypeName, TypeDef>,
    impls: BTreeMap<UsrFuncName, (FuncSig, Vec<Stmt>)>,
    specs: BTreeMap<UsrFuncName, (FuncSig, Vec<Stmt>)>,
    /// a database for inference
    pub infer: InferDatabase,
}

impl ContextWithSig {
    /// Get the generics declaration for a type
    pub fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics> {
        self.types.get(name).map(|def| def.head())
    }

    /// Get type definition
    pub fn get_type_def(&self, name: &UsrTypeName) -> Option<&TypeDef> {
        self.types.get(name)
    }

    /// Parse function body
    pub fn parse_func_body(self) -> Result<ContextWithFunc> {
        // impl
        let mut body_impls = BTreeMap::new();
        for (name, (sig, stmts)) in &self.impls {
            trace!("handling impl body: {}", name);
            let body = ExprParserRoot::new(&self, Kind::Impl, sig).parse(stmts)?;
            trace!("impl body analyzed: {}", name);
            body_impls.insert(name.clone(), body);
        }

        // spec
        let mut body_specs = BTreeMap::new();
        for (name, (sig, stmts)) in &self.specs {
            trace!("handling spec body: {}", name);
            let body = ExprParserRoot::new(&self, Kind::Spec, sig).parse(stmts)?;
            trace!("spec body analyzed: {}", name);
            body_specs.insert(name.clone(), body);
        }

        let Self {
            types,
            impls,
            specs,
            infer: _,
        } = self;

        let unpacked_impls = impls
            .into_iter()
            .map(|(name, (sig, _))| {
                let body = body_impls.remove(&name).unwrap();
                (name, FuncDef::new(sig, body))
            })
            .collect();
        let unpacked_specs = specs
            .into_iter()
            .map(|(name, (sig, _))| {
                let body = body_specs.remove(&name).unwrap();
                (name, FuncDef::new(sig, body))
            })
            .collect();

        Ok(ContextWithFunc {
            types,
            impls: unpacked_impls,
            specs: unpacked_specs,
        })
    }
}

/// Context manager after type, signature, and expression conversion is done
pub struct ContextWithFunc {
    types: BTreeMap<UsrTypeName, TypeDef>,
    impls: BTreeMap<UsrFuncName, FuncDef>,
    specs: BTreeMap<UsrFuncName, FuncDef>,
}

#[cfg(test)]
mod tests {
    use crate::parser::test::unit_test;

    unit_test!(basics, {
        #[smt_type]
        struct SimpleBool(Boolean);
    });
}
