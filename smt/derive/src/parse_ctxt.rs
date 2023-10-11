use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use log::trace;
use syn::{Attribute, Ident, Item, ItemEnum, ItemFn, ItemStruct, Meta, Result, Stmt};
use walkdir::WalkDir;

use crate::err::{bail_on, bail_on_with_note};
use crate::parse_expr::{CtxtForExpr, Expr};
use crate::parse_func::{FuncDef, FuncSig};
use crate::parse_infer::InferDatabase;
use crate::parse_path::{FuncName, TypeName};
use crate::parse_type::{CtxtForType, TypeDef};

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
                "duplicated impl name"
            );
        }
        trace!("spec found: {}", name);
        self.specs.insert(name, item);
        Ok(())
    }

    /// Parse types
    pub fn analyze_type(self) -> Result<ContextWithType> {
        let mut parsed_types = BTreeMap::new();
        for (name, marked) in &self.types {
            trace!("handling type: {}", name);
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

impl CtxtForType for Context {
    fn has_type(&self, name: &TypeName) -> bool {
        self.types.contains_key(name)
    }
}

/// Context manager after type analysis is done
pub struct ContextWithType {
    types: BTreeMap<TypeName, TypeDef>,
    impls: BTreeMap<FuncName, MarkedImpl>,
    specs: BTreeMap<FuncName, MarkedSpec>,
}

impl ContextWithType {
    /// Parse signatures
    pub fn analyze_func_sig(self) -> Result<ContextWithTypeAndSig> {
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

        // register SMT types as they all implement the SMT trait
        for ty in types.keys() {
            infer.register_user_type(ty);
        }
        // register user-implemented functions
        for (name, (sig, _)) in unpacked_impls.iter() {
            infer.register_user_func(name, sig);
        }

        trace!("function database populated with {} entries", infer.size());

        // done
        let ctxt = ContextWithTypeAndSig {
            types,
            impls: unpacked_impls,
            specs: unpacked_specs,
            infer,
        };
        Ok(ctxt)
    }
}

impl CtxtForType for ContextWithType {
    fn has_type(&self, name: &TypeName) -> bool {
        self.types.contains_key(name)
    }
}

/// Context manager after type and signature analysis is done
pub struct ContextWithTypeAndSig {
    types: BTreeMap<TypeName, TypeDef>,
    impls: BTreeMap<FuncName, (FuncSig, Vec<Stmt>)>,
    specs: BTreeMap<FuncName, (FuncSig, Vec<Stmt>)>,
    /// a database for inference
    infer: InferDatabase,
}

impl ContextWithTypeAndSig {
    /// Parse function body
    pub fn analyze_func_body(self) -> Result<ContextWithTypeAndFunc> {
        // impl
        let mut body_impls = BTreeMap::new();
        for (name, (sig, stmts)) in &self.impls {
            trace!("handling impl body: {}", name);
            let body = Expr::from_impl(&self, sig, stmts)?;
            trace!("impl body analyzed: {}", name);
            body_impls.insert(name.clone(), body);
        }

        // spec
        let mut body_specs = BTreeMap::new();
        for (name, (sig, stmts)) in &self.specs {
            trace!("handling spec body: {}", name);
            let body = Expr::from_spec(&self, sig, stmts)?;
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

        Ok(ContextWithTypeAndFunc {
            types,
            impls: unpacked_impls,
            specs: unpacked_specs,
        })
    }
}

impl CtxtForType for ContextWithTypeAndSig {
    fn has_type(&self, name: &TypeName) -> bool {
        self.types.contains_key(name)
    }
}

impl CtxtForExpr for ContextWithTypeAndSig {
    fn get_type(&self, name: &TypeName) -> Option<&TypeDef> {
        self.types.get(name)
    }

    fn get_impl_sig(&self, name: &FuncName) -> Option<&FuncSig> {
        self.impls.get(name).map(|(sig, _)| sig)
    }

    fn get_spec_sig(&self, name: &FuncName) -> Option<&FuncSig> {
        self.specs.get(name).map(|(sig, _)| sig)
    }
}

/// Context manager after type, signature, and expression conversion is done
pub struct ContextWithTypeAndFunc {
    types: BTreeMap<TypeName, TypeDef>,
    impls: BTreeMap<FuncName, FuncDef>,
    specs: BTreeMap<FuncName, FuncDef>,
}
