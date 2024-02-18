use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

use crate::ir::ctxt::IRBuilder;
use crate::ir::index::UsrSortId;
use crate::ir::name::{SmtSortName, UsrSortName};
use crate::parser::infer::TypeRef;
use crate::parser::name::UsrTypeName;
use crate::parser::ty::{EnumVariant, TypeBody, TypeTag};

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

impl Display for Sort {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean => write!(f, "Boolean"),
            Self::Integer => write!(f, "Integer"),
            Self::Rational => write!(f, "Rational"),
            Self::Text => write!(f, "String"),
            Self::Seq(sub) => write!(f, "Vec<{}>", sub),
            Self::Set(sub) => write!(f, "Set<{}>", sub),
            Self::Map(key, val) => write!(f, "Map<{},{}>", key, val),
            Self::Error => write!(f, "Error"),
            Self::User(sid) => write!(f, "${}", sid),
            Self::Uninterpreted(name) => write!(f, "#{}", name),
        }
    }
}

/// A helper enum to represent a variant definition in an ADT type
pub enum Variant {
    Unit,
    Tuple(Vec<Sort>),
    Record(BTreeMap<String, Sort>),
}

impl Display for Variant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Tuple(slots) => {
                let content: Vec<_> = slots.iter().map(|e| e.to_string()).collect();
                write!(f, "({})", content.join(","))
            }
            Self::Record(fields) => {
                let content: Vec<_> = fields.iter().map(|(k, v)| format!("{}:{}", k, v)).collect();
                write!(f, "{{{}}}", content.join(","))
            }
        }
    }
}

/// Complete definition of a sort
pub enum DataType {
    Tuple(Vec<Sort>),
    Record(BTreeMap<String, Sort>),
    Enum(BTreeMap<String, Variant>),
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tuple(slots) => {
                let content: Vec<_> = slots.iter().map(|e| e.to_string()).collect();
                write!(f, "({})", content.join(","))
            }
            Self::Record(fields) => {
                let content: Vec<_> = fields.iter().map(|(k, v)| format!("{}:{}", k, v)).collect();
                write!(f, "{{{}}}", content.join(","))
            }
            Self::Enum(variants) => {
                let content: Vec<_> = variants
                    .iter()
                    .map(|(k, v)| format!("{}{}", k, v))
                    .collect();
                write!(f, "[{}]", content.join(","))
            }
        }
    }
}

/// A registry of data types involved
#[derive(Default)]
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

    /// Get the index given an optional sort name and instantiation
    fn get_index(&self, name: Option<&UsrSortName>, inst: &[Sort]) -> Option<UsrSortId> {
        let idx = match name {
            None => self.idx_tuple.get(inst)?,
            Some(n) => self.idx_named.get(n)?.get(inst)?,
        };
        Some(*idx)
    }

    /// Create a signature in the registry, panic if already registered
    fn create(&mut self, name: Option<UsrSortName>, inst: Vec<Sort>) -> UsrSortId {
        let idx = UsrSortId {
            index: self.idx_tuple.len() + self.idx_named.values().map(|v| v.len()).sum::<usize>(),
        };
        let existing = match name {
            None => self.idx_tuple.insert(inst, idx),
            Some(n) => self.idx_named.entry(n).or_default().insert(inst, idx),
        };
        if existing.is_some() {
            panic!("type instance already registered");
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

    /// Retrieve the data type definition
    pub fn retrieve(&self, idx: UsrSortId) -> &DataType {
        self.defs.get(&idx).expect("no such sort id")
    }

    /// Reverse lookup the sort id to type name and instantiation
    pub fn reverse_lookup(&self, idx: UsrSortId) -> (Option<&UsrSortName>, &[Sort]) {
        for (name, val) in &self.idx_named {
            for (inst, sid) in val {
                if *sid == idx {
                    return (Some(name), inst);
                }
            }
        }
        for (inst, sid) in &self.idx_tuple {
            if *sid == idx {
                return (None, inst);
            }
        }
        panic!("no such sort id");
    }

    /// Retrieve all user-defined data types
    pub fn data_types(&self) -> &BTreeMap<UsrSortId, DataType> {
        &self.defs
    }
}

/// Sort-related functions in the IR builder
impl<'a, 'ctx: 'a> IRBuilder<'a, 'ctx> {
    /// Resolve a type ref to the builder and pull its dependencies into the builder if needed
    pub fn resolve_type(&mut self, ty: &TypeRef) -> Sort {
        let sort = match ty {
            TypeRef::Var(_) => panic!("incomplete type inference"),
            TypeRef::Boolean => Sort::Boolean,
            TypeRef::Integer => Sort::Integer,
            TypeRef::Rational => Sort::Rational,
            TypeRef::Text => Sort::Text,
            TypeRef::Cloak(sub) => {
                // unwrap the cloak
                self.resolve_type(sub.as_ref())
            }
            TypeRef::Seq(sub) => Sort::Seq(self.resolve_type(sub.as_ref()).into()),
            TypeRef::Set(sub) => Sort::Set(self.resolve_type(sub.as_ref()).into()),
            TypeRef::Map(key, val) => Sort::Map(
                self.resolve_type(key.as_ref()).into(),
                self.resolve_type(val.as_ref()).into(),
            ),
            TypeRef::Error => Sort::Error,
            TypeRef::User(name, inst) => Sort::User(self.register_type(Some(name), inst)),
            TypeRef::Pack(elems) => Sort::User(self.register_type(None, elems)),
            TypeRef::Parameter(name) => self
                .ty_inst
                .get(name)
                .unwrap_or_else(|| panic!("no such type parameter {}", name))
                .clone(),
        };
        sort
    }

    /// Utility: resolve a vec of type refs
    pub fn resolve_type_ref_vec(&mut self, tys: &[TypeRef]) -> Vec<Sort> {
        tys.iter().map(|e| self.resolve_type(e)).collect()
    }

    /// Utility: resolve a map of type refs
    fn resolve_type_ref_map(&mut self, tys: &BTreeMap<String, TypeRef>) -> BTreeMap<String, Sort> {
        tys.iter()
            .map(|(key, val)| (key.clone(), self.resolve_type(val)))
            .collect()
    }

    /// Utility: resolve a vec of type tags
    fn resolve_type_tag_vec(&mut self, tys: &[TypeTag]) -> Vec<Sort> {
        tys.iter().map(|e| self.resolve_type(&e.into())).collect()
    }

    /// Utility: resolve a map of type tags
    fn resolve_type_tag_map(&mut self, tys: &BTreeMap<String, TypeTag>) -> BTreeMap<String, Sort> {
        tys.iter()
            .map(|(key, val)| (key.clone(), self.resolve_type(&val.into())))
            .collect()
    }

    /// Register an instantiated type definition and its dependencies (if not previously registered)
    pub fn register_type(
        &mut self,
        ty_name: Option<&UsrTypeName>,
        ty_args: &[TypeRef],
    ) -> UsrSortId {
        let name = ty_name.map(|n| n.into());
        let ty_args = self.resolve_type_ref_vec(ty_args);

        // check if we have already processed the data type
        match self.ir.ty_registry.get_index(name.as_ref(), &ty_args) {
            None => (),
            Some(idx) => return idx,
        }

        // create the instance and get the index
        let idx = self.ir.ty_registry.create(name, ty_args.clone());

        // process the definition
        let def = match ty_name {
            None => {
                // construct a tuple without conversion needed
                DataType::Tuple(ty_args)
            }
            Some(n) => {
                let def = self.ctxt.get_type(n);

                // prepare the builder for definition processing
                let mut builder = self.derive(&def.head, ty_args);

                // parse the definition with the newly contextualized holder
                match &def.body {
                    TypeBody::Tuple(tuple) => {
                        DataType::Tuple(builder.resolve_type_tag_vec(&tuple.slots))
                    }
                    TypeBody::Record(record) => {
                        DataType::Record(builder.resolve_type_tag_map(&record.fields))
                    }
                    TypeBody::Enum(adt) => {
                        let mut variants = BTreeMap::new();
                        for (key, val) in &adt.variants {
                            let variant = match val {
                                EnumVariant::Unit => Variant::Unit,
                                EnumVariant::Tuple(tuple) => {
                                    Variant::Tuple(builder.resolve_type_tag_vec(&tuple.slots))
                                }
                                EnumVariant::Record(record) => {
                                    Variant::Record(builder.resolve_type_tag_map(&record.fields))
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
        self.ir.ty_registry.register_def(idx, def);

        // return the sort
        idx
    }

    /// Lookup the user sort id given an optional sort name and instantiation
    pub fn lookup_type(&self, name: Option<&UsrSortName>, inst: &[Sort]) -> UsrSortId {
        match self.ir.ty_registry.get_index(name, inst) {
            None => {
                let inst_content = inst
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                match name {
                    None => panic!("anonymous sort not registered ({})", inst_content),
                    Some(n) => panic!("user-defined sort not registered {}<{}>", n, inst_content),
                }
            }
            Some(sid) => sid,
        }
    }
}
