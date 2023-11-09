use std::collections::BTreeMap;

use anyhow::{anyhow, bail, Result};

use crate::ir::name::{index, name};
use crate::parser::ctxt::ContextWithFunc;
use crate::parser::infer::TypeRef;
use crate::parser::name::{TypeParamName, UsrTypeName};
use crate::parser::ty::{EnumVariant, TypeBody, TypeTag};

name! {
    /// Name of a type parameter that implements the SMT trait
    SmtSortName
}

impl SmtSortName {
    /// Name for an uninterpreted sort
    pub fn new(name: &TypeParamName) -> Self {
        Self {
            ident: name.to_string(),
        }
    }
}

name! {
    /// Name of a user-defined sort
    UsrSortName
}

index! {
    /// A unique identifier for user-defined sort
    UsrSortId
}

/// A unique and complete reference to an SMT sort
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Sort {
    /// boolean
    Boolean,
    /// integer (unlimited precision)
    Integer,
    /// rational numbers (unlimited precision)
    Rational,
    /// string
    Text,
    /// SMT-sequence
    Seq(Box<Sort>),
    /// SMT-set
    Set(Box<Sort>),
    /// SMT-array
    Map(Box<Sort>, Box<Sort>),
    /// dynamic error type
    Error,
    /// user-defined type (including pack-defined type tuple)
    User(UsrSortId),
    /// uninterpreted
    Uninterpreted(SmtSortName),
}

/// A helper enum to represent a variant definition in an ADT type
pub enum Variant {
    Unit,
    Tuple(Vec<Sort>),
    Record(BTreeMap<String, Sort>),
}

/// Complete definition of a sort
pub enum DataType {
    Tuple(Vec<Sort>),
    Record(BTreeMap<String, Sort>),
    Enum(BTreeMap<String, Variant>),
}

/// A registry of data types involved
pub struct TypeRegistry {
    /// a map from user-defined type and instantiations to sort id
    idx_named: BTreeMap<UsrSortName, BTreeMap<Vec<Sort>, UsrSortId>>,
    /// a map from unnamed type tuple (still user-defined) to sort id
    idx_tuple: BTreeMap<Vec<Sort>, UsrSortId>,
    /// the actual type definitions
    defs: BTreeMap<UsrSortId, DataType>,
}

impl TypeRegistry {
    /// Initialize an empty registry
    pub fn new() -> Self {
        Self {
            idx_named: BTreeMap::new(),
            idx_tuple: BTreeMap::new(),
            defs: BTreeMap::new(),
        }
    }

    /// Get the index given a name and instantiation
    fn get_index(&self, name: Option<&UsrSortName>, inst: &[Sort]) -> Option<UsrSortId> {
        let idx = match name {
            None => self.idx_tuple.get(inst)?,
            Some(n) => self.idx_named.get(n)?.get(inst)?,
        };
        Some(*idx)
    }

    /// Register a signature to the registry
    fn register_sig(&mut self, name: Option<UsrSortName>, inst: Vec<Sort>) -> UsrSortId {
        let idx = UsrSortId {
            index: self.idx_tuple.len() + self.idx_named.values().map(|v| v.len()).sum::<usize>(),
        };
        let existing = match name {
            None => self.idx_tuple.insert(inst, idx),
            Some(n) => self.idx_named.entry(n).or_default().insert(inst, idx),
        };
        if existing.is_some() {
            panic!("type signature already registered");
        }
        idx
    }

    /// Register a definition to the registry
    fn register_def(&mut self, idx: UsrSortId, def: DataType) {
        let existing = self.defs.insert(idx, def);
        if existing.is_some() {
            panic!("type definition already registered");
        }
    }
}

/// A contextualized holder for the type registry
pub struct TypeRegistryHolder<'a, 'ctx: 'a> {
    /// information provider
    ctxt: &'ctx ContextWithFunc,
    /// type instantiation in the current context
    ty_args: BTreeMap<TypeParamName, Sort>,
    /// type registry to be modified
    registry: &'a mut TypeRegistry,
}

/// Sort-related functions in the IR builder
impl<'a, 'ctx: 'a> TypeRegistryHolder<'a, 'ctx> {
    /// Create a new holder
    pub fn new(
        ctxt: &'ctx ContextWithFunc,
        ty_args: BTreeMap<TypeParamName, Sort>,
        registry: &'a mut TypeRegistry,
    ) -> Self {
        Self {
            ctxt,
            ty_args,
            registry,
        }
    }

    /// Resolve a type ref to the builder and pull its dependencies into the builder if needed
    pub fn resolve(&mut self, ty: &TypeRef) -> Result<Sort> {
        let sort = match ty {
            TypeRef::Var(_) => bail!("incomplete type"),
            TypeRef::Boolean => Sort::Boolean,
            TypeRef::Integer => Sort::Integer,
            TypeRef::Rational => Sort::Rational,
            TypeRef::Text => Sort::Text,
            TypeRef::Cloak(sub) => {
                // unwrap the cloak
                self.resolve(sub.as_ref())?
            }
            TypeRef::Seq(sub) => Sort::Seq(self.resolve(sub.as_ref())?.into()),
            TypeRef::Set(sub) => Sort::Set(self.resolve(sub.as_ref())?.into()),
            TypeRef::Map(key, val) => Sort::Map(
                self.resolve(key.as_ref())?.into(),
                self.resolve(val.as_ref())?.into(),
            ),
            TypeRef::Error => Sort::Error,
            TypeRef::User(name, inst) => self.register(Some(name), inst)?,
            TypeRef::Pack(elems) => self.register(None, elems)?,
            TypeRef::Parameter(name) => self
                .ty_args
                .get(name)
                .ok_or_else(|| anyhow!("no such type parameter {}", name))?
                .clone(),
        };
        Ok(sort)
    }

    /// Utility for resolving a vector of type refs
    fn resolve_ref_vec(&mut self, tys: &[TypeRef]) -> Result<Vec<Sort>> {
        tys.iter().map(|e| self.resolve(e)).collect::<Result<_>>()
    }

    /// Utility for resolving a map of type refs
    fn resolve_ref_map(
        &mut self,
        tys: &BTreeMap<String, TypeRef>,
    ) -> Result<BTreeMap<String, Sort>> {
        let mut fields = BTreeMap::new();
        for (key, val) in tys {
            fields.insert(key.clone(), self.resolve(val)?);
        }
        Ok(fields)
    }

    /// Utility for resolving a vector of type tags
    fn resolve_tag_vec(&mut self, tys: &[TypeTag]) -> Result<Vec<Sort>> {
        tys.iter()
            .map(|e| self.resolve(&e.into()))
            .collect::<Result<_>>()
    }

    /// Utility for resolving a vector of type refs
    fn resolve_tag_map(
        &mut self,
        tys: &BTreeMap<String, TypeTag>,
    ) -> Result<BTreeMap<String, Sort>> {
        let mut fields = BTreeMap::new();
        for (key, val) in tys {
            fields.insert(key.clone(), self.resolve(&val.into())?);
        }
        Ok(fields)
    }

    /// Register an instantiated type definition and its dependencies (if not previously registered)
    fn register(&mut self, ty_name: Option<&UsrTypeName>, ty_inst: &[TypeRef]) -> Result<Sort> {
        let name = ty_name.map(|n| UsrSortName {
            ident: n.to_string(),
        });
        let args = self.resolve_ref_vec(ty_inst)?;

        // check if we have already processed the data type
        match self.registry.get_index(name.as_ref(), &args) {
            None => (),
            Some(index) => return Ok(Sort::User(index)),
        }

        // register the signature and get the index
        let idx = self.registry.register_sig(name, args.clone());

        // process the definition
        let def = match ty_name {
            None => {
                // construct a tuple without conversion needed
                DataType::Tuple(args)
            }
            Some(n) => {
                let def = self.ctxt.get_type(n);

                // prepare the holder for definition processing
                let params = &def.head.params;
                if params.len() != args.len() {
                    bail!("generics mismatch");
                }

                let mut ty_args = BTreeMap::new();
                for (param, arg) in params.iter().zip(args.iter()) {
                    match ty_args.insert(param.clone(), arg.clone()) {
                        None => (),
                        Some(_) => bail!("duplicated type parameter {}", param),
                    }
                }

                let mut holder = TypeRegistryHolder::new(self.ctxt, ty_args, self.registry);

                // parse the definition with the newly contextualized holder
                match &def.body {
                    TypeBody::Tuple(tuple) => {
                        DataType::Tuple(holder.resolve_tag_vec(&tuple.slots)?)
                    }
                    TypeBody::Record(record) => {
                        DataType::Record(holder.resolve_tag_map(&record.fields)?)
                    }
                    TypeBody::Enum(adt) => {
                        let mut variants = BTreeMap::new();
                        for (key, val) in &adt.variants {
                            let variant = match val {
                                EnumVariant::Unit => Variant::Unit,
                                EnumVariant::Tuple(tuple) => {
                                    Variant::Tuple(holder.resolve_tag_vec(&tuple.slots)?)
                                }
                                EnumVariant::Record(record) => {
                                    Variant::Record(holder.resolve_tag_map(&record.fields)?)
                                }
                            };
                            variants.insert(key.clone(), variant);
                        }
                        DataType::Enum(variants)
                    }
                }
            }
        };

        // register the definition
        self.registry.register_def(idx, def);

        // return the sort
        Ok(Sort::User(idx))
    }
}
