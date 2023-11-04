use std::collections::BTreeMap;

use anyhow::{anyhow, bail, Result};
use petgraph::graph::{DiGraph, NodeIndex};

use crate::ir::name::name;
use crate::parser::ctxt::ContextWithFunc;
use crate::parser::infer::TypeRef;
use crate::parser::name::TypeParamName;
use crate::parser::ty::TypeDef;

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
    /// a tuple of types
    Pack(Vec<Sort>),
    /// user-defined type
    User(UsrSortName, Vec<Sort>),
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
    /// map from user-defined types to node indices
    user_defined: BTreeMap<UsrSortName, BTreeMap<Vec<Sort>, NodeIndex>>,
    /// map from packed type tuples to node indices
    pack_defined: BTreeMap<Vec<Sort>, NodeIndex>,
    /// a graph hosting all data types and their dependency relation
    graph: DiGraph<DataType, ()>,
}

impl TypeRegistry {
    /// Initialize an empty registry
    pub fn new() -> Self {
        Self {
            user_defined: BTreeMap::new(),
            pack_defined: BTreeMap::new(),
            graph: DiGraph::new(),
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
            TypeRef::User(name, args) => {
                // register the type
                let sorts: Vec<_> = args
                    .iter()
                    .map(|e| self.resolve(e))
                    .collect::<Result<_>>()?;
                let def = self.ctxt.get_type(name);
                self.register(def, &sorts)?;

                // return the sort
                Sort::User(
                    UsrSortName {
                        ident: name.to_string(),
                    },
                    sorts,
                )
            }
            TypeRef::Pack(elems) => Sort::Pack(
                elems
                    .iter()
                    .map(|e| self.resolve(e))
                    .collect::<Result<_>>()?,
            ),
            TypeRef::Parameter(name) => self
                .ty_args
                .get(name)
                .ok_or_else(|| anyhow!("no such type parameter {}", name))?
                .clone(),
        };
        Ok(sort)
    }

    /// Register an instantiated type definition and its dependencies (if not previously registered)
    fn register(&mut self, def: &TypeDef, args: &[Sort]) -> Result<()> {
        Ok(())
    }
}
