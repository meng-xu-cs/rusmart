use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter};
use std::fs;
use std::path::Path;

use log::trace;
use syn::{File, Ident, Item, ItemConst, ItemEnum, ItemFn, ItemMod, ItemStruct, Result, Stmt};
use walkdir::WalkDir;

use crate::parser::apply::{ApplyDatabase, Kind};
use crate::parser::attr::{ImplMark, Mark, SpecMark};
use crate::parser::err::{bail_if_exists, bail_on, bail_on_with_note};
use crate::parser::expr::ExprParserRoot;
use crate::parser::func::{Axiom, FuncSig, ImplFuncDef, SpecFuncDef};
use crate::parser::generics::Generics;
use crate::parser::name::{UsrFuncName, UsrTypeName};
use crate::parser::ty::{TypeBody, TypeDef};

#[cfg(test)]
use proc_macro2::TokenStream;

/// SMT-marked type
pub enum MarkedType {
    Enum(ItemEnum),
    Struct(ItemStruct),
}

impl MarkedType {
    /// Retrieve the name of this item
    pub fn name(&self) -> &Ident {
        match self {
            Self::Enum(item) => &item.ident,
            Self::Struct(item) => &item.ident,
        }
    }
}

/// SMT-marked function as impl
pub struct MarkedImpl {
    item: ItemFn,
    mark: ImplMark,
}

impl MarkedImpl {
    /// Retrieve the name of this item
    pub fn name(&self) -> &Ident {
        &self.item.sig.ident
    }
}

/// SMT-marked function as spec
pub struct MarkedSpec {
    item: ItemFn,
    mark: SpecMark,
}

impl MarkedSpec {
    /// Retrieve the name of this item
    pub fn name(&self) -> &Ident {
        &self.item.sig.ident
    }
}

/// SMT-marked const as axiom
pub struct MarkedAxiom {
    item: ItemConst,
}

impl MarkedAxiom {
    /// Retrieve the name of this item
    pub fn name(&self) -> &Ident {
        &self.item.ident
    }
}

/// A refinement relation
#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct Refinement {
    fn_impl: UsrFuncName,
    fn_spec: UsrFuncName,
}

impl Display for Refinement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ~> {}", self.fn_impl, self.fn_spec)
    }
}

/// Context manager for holding marked items
pub struct Context {
    types: BTreeMap<UsrTypeName, MarkedType>,
    impls: BTreeMap<UsrFuncName, MarkedImpl>,
    specs: BTreeMap<UsrFuncName, MarkedSpec>,
    axioms: Vec<MarkedAxiom>,
}

impl Context {
    /// Build a context for crate
    pub fn new(path_input: &Path) -> Result<Self> {
        let mut ctxt = Self {
            types: BTreeMap::new(),
            impls: BTreeMap::new(),
            specs: BTreeMap::new(),
            axioms: vec![],
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

        // post-collection checking
        ctxt.sanity_check()?;

        // return a collection of items as derivation context
        Ok(ctxt)
    }

    /// Process a list of items extracted from a file or mod
    fn process_items(&mut self, items: Vec<Item>) -> Result<()> {
        // identify items marked with smt-related attributes
        for item in items {
            match item {
                Item::Enum(syntax) => match Mark::parse_attrs(&syntax.attrs)? {
                    None => continue,
                    Some(Mark::Type) => self.add_type(MarkedType::Enum(syntax))?,
                    _ => bail_on!(syntax, "invalid annotation"),
                },
                Item::Struct(syntax) => match Mark::parse_attrs(&syntax.attrs)? {
                    None => continue,
                    Some(Mark::Type) => self.add_type(MarkedType::Struct(syntax))?,
                    _ => bail_on!(syntax, "invalid annotation"),
                },
                Item::Fn(syntax) => match Mark::parse_attrs(&syntax.attrs)? {
                    None => continue,
                    Some(Mark::Impl(mark)) => self.add_impl(MarkedImpl { item: syntax, mark })?,
                    Some(Mark::Spec(mark)) => self.add_spec(MarkedSpec { item: syntax, mark })?,
                    _ => bail_on!(syntax, "invalid annotation"),
                },
                Item::Const(syntax) => match Mark::parse_attrs(&syntax.attrs)? {
                    None => continue,
                    Some(Mark::Axiom) => self.add_axiom(MarkedAxiom { item: syntax })?,
                    _ => bail_on!(syntax, "invalid annotation"),
                },
                Item::Mod(syntax) => {
                    let ItemMod {
                        attrs: _,
                        vis: _,
                        unsafety,
                        mod_token: _,
                        ident: _,
                        content,
                        semi,
                    } = syntax;
                    bail_if_exists!(unsafety);
                    match content {
                        None => (),
                        Some((_, items)) => {
                            bail_if_exists!(semi);
                            self.process_items(items)?
                        }
                    }
                }
                _ => (),
            }
        }
        Ok(())
    }

    /// Process a single input file
    fn process_file(&mut self, file: File) -> Result<()> {
        let File {
            shebang: _,
            attrs: _,
            items,
        } = file;
        self.process_items(items)
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

    /// Add an axiom to the context
    fn add_axiom(&mut self, item: MarkedAxiom) -> Result<()> {
        let name = item.name();
        if *name != "_" {
            bail_on!(name, "expect _");
        }
        self.axioms.push(item);
        Ok(())
    }

    /// Check whether the marks declared are correct or not
    fn sanity_check(&self) -> Result<()> {
        for marked in self.impls.values() {
            let MarkedImpl { item, mark } = marked;
            for target in &mark.specs {
                if !self.specs.contains_key(target) {
                    bail_on!(item, "invalid spec target: {}", target);
                }
            }
        }
        for marked in self.specs.values() {
            let MarkedSpec { item, mark } = marked;
            for target in &mark.impls {
                if !self.impls.contains_key(target) {
                    bail_on!(item, "invalid impl target: {}", target);
                }
            }
        }
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
            axioms: self.axioms,
        })
    }

    #[cfg(test)]
    pub fn new_from_stream(stream: TokenStream) -> Result<Self> {
        let mut ctxt = Self {
            types: BTreeMap::new(),
            impls: BTreeMap::new(),
            specs: BTreeMap::new(),
            axioms: vec![],
        };

        let file: File = syn::parse2(stream)?;
        ctxt.process_file(file)?;
        ctxt.sanity_check()?;
        Ok(ctxt)
    }
}

/// Context manager after analyzing for generics
pub struct ContextWithGenerics {
    types: BTreeMap<UsrTypeName, (Generics, MarkedType)>,
    impls: BTreeMap<UsrFuncName, MarkedImpl>,
    specs: BTreeMap<UsrFuncName, MarkedSpec>,
    axioms: Vec<MarkedAxiom>,
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
            axioms,
        } = self;

        let new_types = types
            .into_iter()
            .map(|(name, (generics, _))| {
                let body = parsed_types.remove(&name).unwrap();
                let def = TypeDef {
                    head: generics,
                    body,
                };
                (name, def)
            })
            .collect();

        Ok(ContextWithType {
            types: new_types,
            impls,
            specs,
            axioms,
        })
    }
}

/// Context manager after type analysis is done
pub struct ContextWithType {
    types: BTreeMap<UsrTypeName, TypeDef>,
    impls: BTreeMap<UsrFuncName, MarkedImpl>,
    specs: BTreeMap<UsrFuncName, MarkedSpec>,
    axioms: Vec<MarkedAxiom>,
}

impl ContextWithType {
    /// Get the generics declaration for a type
    pub fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics> {
        self.types.get(name).map(|def| &def.head)
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
            } = &marked.item;

            trace!("handling impl sig: {}", name);
            let parsed = FuncSig::from_sig(&self, sig)?;
            trace!("impl sig analyzed: {}", name);
            sig_impls.insert(name.clone(), (parsed, sig.clone()));
        }

        // spec
        let mut sig_specs = BTreeMap::new();
        for (name, marked) in &self.specs {
            let ItemFn {
                attrs: _,
                vis: _,
                sig,
                block: _, // handled later
            } = &marked.item;

            trace!("handling spec sig: {}", name);
            let parsed = FuncSig::from_sig(&self, sig)?;
            trace!("spec sig analyzed: {}", name);
            sig_specs.insert(name.clone(), (parsed, sig.clone()));
        }

        // axiom
        let mut unpacked_axioms = vec![];
        for (i, ast) in self.axioms.iter().enumerate() {
            trace!("handling axiom sig: {}", i);
            let (sig, body) = FuncSig::unpack_axiom(&self, &ast.item)?;
            trace!("axiom sig analyzed: {}", i);
            unpacked_axioms.push((sig, body));
        }

        // populate the databases
        let mut vc_db = BTreeSet::new();
        let mut fn_db = ApplyDatabase::with_intrinsics();

        for (name, (sig, raw)) in sig_impls.iter() {
            let mark = &self.impls.get(name).expect("impl").mark;
            // check signature
            for spec_name in &mark.specs {
                let (spec_sig, _) = sig_specs.get(spec_name).expect("spec");
                if !spec_sig.is_compatible(sig) {
                    bail_on!(raw, "signature mismatch");
                }
                vc_db.insert(Refinement {
                    fn_impl: name.clone(),
                    fn_spec: spec_name.clone(),
                });
            }

            // register to type db
            match fn_db.register_user_func(name, mark.method.as_ref(), sig, Kind::Impl) {
                Ok(()) => (),
                Err(e) => bail_on!(raw, "{}", e),
            }
        }

        for (name, (sig, raw)) in sig_specs.iter() {
            let mark = &self.specs.get(name).expect("spec").mark;
            // check signature
            for impl_name in &mark.impls {
                let (impl_sig, _) = sig_impls.get(impl_name).expect("impl");
                if !impl_sig.is_compatible(sig) {
                    bail_on!(raw, "signature mismatch");
                }
                vc_db.insert(Refinement {
                    fn_impl: impl_name.clone(),
                    fn_spec: name.clone(),
                });
            }

            // register to type db
            match fn_db.register_user_func(name, mark.method.as_ref(), sig, Kind::Spec) {
                Ok(()) => (),
                Err(e) => bail_on!(raw, "{}", e),
            }
        }
        trace!("databases constructed");

        // re-packing
        let Self {
            types,
            impls,
            specs,
            axioms: _,
        } = self;

        let unpacked_impls: BTreeMap<_, _> = impls
            .into_iter()
            .map(|(name, marked)| {
                let (sig, _) = sig_impls.remove(&name).unwrap();
                let stmts = marked.item.block.stmts;
                (name, (sig, stmts))
            })
            .collect();
        let unpacked_specs: BTreeMap<_, _> = specs
            .into_iter()
            .map(|(name, marked)| {
                let (sig, _) = sig_specs.remove(&name).unwrap();
                let stmts = marked.item.block.stmts;
                (name, (sig, stmts))
            })
            .collect();

        // done
        let ctxt = ContextWithSig {
            types,
            impls: unpacked_impls,
            specs: unpacked_specs,
            axioms: unpacked_axioms,
            vc_db,
            fn_db,
        };
        Ok(ctxt)
    }
}

/// Context manager after type and function signature analysis is done
pub struct ContextWithSig {
    types: BTreeMap<UsrTypeName, TypeDef>,
    impls: BTreeMap<UsrFuncName, (FuncSig, Vec<Stmt>)>,
    specs: BTreeMap<UsrFuncName, (FuncSig, Vec<Stmt>)>,
    axioms: Vec<(FuncSig, Vec<Stmt>)>,
    /// a database for verification conditions (i.e., impl and spec mapping)
    vc_db: BTreeSet<Refinement>,
    /// a database for functions
    pub fn_db: ApplyDatabase,
}

impl ContextWithSig {
    /// Get the generics declaration for a type
    pub fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics> {
        self.types.get(name).map(|def| &def.head)
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
            // check for uninterpreted function
            let uninterpreted = Axiom::is_unimplemented(stmts)?;
            let body = if uninterpreted {
                None
            } else {
                Some(ExprParserRoot::new(&self, Kind::Spec, sig).parse(stmts)?)
            };
            trace!("spec body analyzed: {}", name);
            body_specs.insert(name.clone(), body);
        }

        // axiom
        let mut parsed_axioms = vec![];
        for (i, (sig, stmts)) in self.axioms.iter().enumerate() {
            trace!("handling axiom body: {}", i);
            let body = ExprParserRoot::new(&self, Kind::Spec, sig).parse(stmts)?;
            trace!("axiom body analyzed: {}", i);
            parsed_axioms.push(body);
        }

        // repacking
        let Self {
            types,
            impls,
            specs,
            axioms,
            fn_db: _,
            vc_db,
        } = self;

        let unpacked_impls = impls
            .into_iter()
            .map(|(name, (sig, _))| {
                let body = body_impls.remove(&name).unwrap();
                (name, ImplFuncDef { head: sig, body })
            })
            .collect();
        let unpacked_specs = specs
            .into_iter()
            .map(|(name, (sig, _))| {
                let body = body_specs.remove(&name).unwrap();
                (name, SpecFuncDef { head: sig, body })
            })
            .collect();
        let unpack_axioms = axioms
            .into_iter()
            .zip(parsed_axioms)
            .map(|((sig, _), body)| Axiom { head: sig, body })
            .collect();

        Ok(ContextWithFunc {
            types,
            impls: unpacked_impls,
            specs: unpacked_specs,
            axioms: unpack_axioms,
            vc_db,
        })
    }
}

/// Context manager after type, signature, and expression conversion is done
pub struct ContextWithFunc {
    types: BTreeMap<UsrTypeName, TypeDef>,
    impls: BTreeMap<UsrFuncName, ImplFuncDef>,
    specs: BTreeMap<UsrFuncName, SpecFuncDef>,
    axioms: Vec<Axiom>,
    vc_db: BTreeSet<Refinement>,
}

impl ContextWithFunc {
    /// Enumerate over the verification conditions
    pub fn refinements(&self) -> impl Iterator<Item = &Refinement> {
        self.vc_db.iter()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test::unit_test;

    unit_test!(basics, {
        #[smt_type]
        struct SimpleBool(Boolean);
    });
}
