use std::collections::BTreeMap;

use syn::{
    AngleBracketedGenericArguments, Field, Fields, FieldsNamed, FieldsUnnamed, GenericArgument,
    ItemEnum, ItemStruct, Path, PathArguments, PathSegment, Result, Type, TypePath, TypeReference,
    Variant,
};

use crate::parse_ctxt::{bail_if_exists, bail_if_missing, bail_on, Context, MarkedType, TypeName};

/// A context suitable for type analysis
pub trait CtxtForType {
    fn has_type(&self, name: &TypeName) -> bool;
}

/// A unique and complete reference to an SMT-related type
pub enum TypeTag {
    /// boolean
    Boolean,
    /// integer (unlimited precision)
    Integer,
    /// rational numbers (unlimited precision)
    Rational,
    /// string
    Text,
    /// inductively defined type
    Box(Box<TypeTag>),
    /// SMT-sequence
    Seq(Box<TypeTag>),
    /// SMT-set
    Set(Box<TypeTag>),
    /// SMT-array
    Map(Box<TypeTag>, Box<TypeTag>),
    /// dynamic error type
    Error,
    /// user-defined type
    User(TypeName),
}

impl TypeTag {
    /// Convert from a type argument pack
    fn from_args<T: CtxtForType>(
        ctxt: &T,
        pack: &AngleBracketedGenericArguments,
    ) -> Result<Vec<Self>> {
        let AngleBracketedGenericArguments {
            colon2_token,
            args,
            lt_token: _,
            gt_token: _,
        } = pack;
        bail_if_exists!(colon2_token);

        let mut list = vec![];
        for arg in args {
            match arg {
                GenericArgument::Type(ty) => {
                    list.push(Self::from_type(ctxt, ty)?);
                }
                _ => bail_on!(arg, "invalid type argument"),
            }
        }
        Ok(list)
    }

    /// Convert from a type argument pack, expecting 1 argument
    fn from_args_expect_1<T: CtxtForType>(
        ctxt: &T,
        pack: &AngleBracketedGenericArguments,
    ) -> Result<Self> {
        let mut iter = Self::from_args(ctxt, pack)?.into_iter();
        let a1 = match iter.next() {
            None => bail_on!(pack, "expect 1 argument, found 0"),
            Some(t) => t,
        };
        if iter.next().is_some() {
            bail_on!(pack, "expect 1 argument, found 2+");
        }
        Ok(a1)
    }

    /// Convert from a type argument pack, expecting 2 arguments
    fn from_args_expect_2<T: CtxtForType>(
        ctxt: &T,
        pack: &AngleBracketedGenericArguments,
    ) -> Result<(Self, Self)> {
        let mut iter = Self::from_args(ctxt, pack)?.into_iter();
        let a1 = match iter.next() {
            None => bail_on!(pack, "expect 2 argument, found 0"),
            Some(t) => t,
        };
        let a2 = match iter.next() {
            None => bail_on!(pack, "expect 2 argument, found 1"),
            Some(t) => t,
        };
        if iter.next().is_some() {
            bail_on!(pack, "expect 2 argument, found 3+");
        }
        Ok((a1, a2))
    }

    /// Convert from a path
    fn from_path<T: CtxtForType>(ctxt: &T, path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        // in reverse order
        let mut iter = segments.iter().rev();
        let tag = match iter.next() {
            None => bail_on!(segments, "invalid path"),
            Some(segment) => {
                let PathSegment { ident, arguments } = segment;
                match (ident.to_string().as_str(), arguments) {
                    ("Boolean", PathArguments::None) => Self::Boolean,
                    ("Integer", PathArguments::None) => Self::Integer,
                    ("Rational", PathArguments::None) => Self::Rational,
                    ("Text", PathArguments::None) => Self::Text,
                    ("Box", PathArguments::AngleBracketed(pack)) => {
                        let sub = Self::from_args_expect_1(ctxt, pack)?;
                        Self::Box(sub.into())
                    }
                    ("Seq", PathArguments::AngleBracketed(pack)) => {
                        let sub = Self::from_args_expect_1(ctxt, pack)?;
                        Self::Seq(sub.into())
                    }
                    ("Set", PathArguments::AngleBracketed(pack)) => {
                        let sub = Self::from_args_expect_1(ctxt, pack)?;
                        Self::Set(sub.into())
                    }
                    ("Map", PathArguments::AngleBracketed(pack)) => {
                        let (key, val) = Self::from_args_expect_2(ctxt, pack)?;
                        Self::Map(key.into(), val.into())
                    }
                    ("Error", PathArguments::None) => Self::Error,
                    (_, _) => {
                        let name = ident.try_into()?;
                        if !ctxt.has_type(&name) {
                            bail_on!(ident, "no such type");
                        }
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected type arguments");
                        }
                        Self::User(name)
                    }
                }
            }
        };
        Ok(tag)
    }

    /// Convert from a type
    pub fn from_type<T: CtxtForType>(ctxt: &T, ty: &Type) -> Result<Self> {
        match ty {
            Type::Path(TypePath { qself, path }) => {
                bail_if_exists!(qself);
                Self::from_path(ctxt, path)
            }
            _ => bail_on!(ty, "expect type path"),
        }
    }
}

/// Represents a tuple definition
pub struct TypeTuple {
    slots: Vec<TypeTag>,
}

impl TypeTuple {
    /// Convert from a list of fields
    fn from_fields<'a, I: Iterator<Item = &'a Field>, T: CtxtForType>(
        ctxt: &T,
        items: I,
    ) -> Result<Self> {
        let mut slots = vec![];

        for field in items {
            let Field {
                attrs: _,
                vis: _,
                mutability: _,
                ident,
                colon_token,
                ty,
            } = field;

            bail_if_exists!(ident);
            bail_if_exists!(colon_token);

            let tag = TypeTag::from_type(ctxt, ty)?;
            slots.push(tag);
        }

        Ok(Self { slots })
    }
}

/// Represents a record definition
pub struct TypeRecord {
    fields: BTreeMap<String, TypeTag>,
}

impl TypeRecord {
    /// Convert from a fields token
    fn from_fields<'a, I: Iterator<Item = &'a Field>, T: CtxtForType>(
        ctxt: &T,
        items: I,
    ) -> Result<Self> {
        let mut fields = BTreeMap::new();

        for field in items {
            let Field {
                attrs: _,
                vis: _,
                mutability: _,
                ident,
                colon_token,
                ty,
            } = field;

            let name = bail_if_missing!(ident, field, "name");
            bail_if_missing!(colon_token, field, "colon");

            let tag = TypeTag::from_type(ctxt, ty)?;
            match fields.insert(name.to_string(), tag) {
                None => (),
                Some(_) => bail_on!(ident, "duplicated field name"),
            }
        }

        Ok(Self { fields })
    }
}

/// A helper enum to represent a variant definition in an ADT type
pub enum ADTVariant {
    Unit,
    Tuple(TypeTuple),
    Record(TypeRecord),
}

impl ADTVariant {
    /// Convert from a fields token
    fn from_fields<T: CtxtForType>(ctxt: &T, fields: &Fields) -> Result<Self> {
        let variant = match fields {
            Fields::Unit => Self::Unit,
            Fields::Named(FieldsNamed {
                brace_token: _,
                named,
            }) => Self::Record(TypeRecord::from_fields(ctxt, named.iter())?),
            Fields::Unnamed(FieldsUnnamed {
                paren_token: _,
                unnamed,
            }) => Self::Tuple(TypeTuple::from_fields(ctxt, unnamed.iter())?),
        };
        Ok(variant)
    }
}

/// Represents an ADT definition
pub struct TypeADT {
    variants: BTreeMap<String, ADTVariant>,
}

impl TypeADT {
    /// Convert from a list of variants
    fn from_variants<'a, I: Iterator<Item = &'a Variant>, T: CtxtForType>(
        ctxt: &T,
        items: I,
    ) -> Result<Self> {
        let mut variants = BTreeMap::new();

        for variant in items {
            let Variant {
                attrs: _,
                ident,
                fields,
                discriminant,
            } = variant;

            bail_if_exists!(discriminant.as_ref().map(|(_, e)| e));
            let branch = ADTVariant::from_fields(ctxt, fields)?;
            match variants.insert(ident.to_string(), branch) {
                None => (),
                Some(_) => bail_on!(ident, "duplicated variant name"),
            }
        }

        Ok(Self { variants })
    }
}

/// A complete definition of type
pub enum TypeDef {
    Tuple(TypeTuple),
    Record(TypeRecord),
    Algebraic(TypeADT),
}

impl TypeDef {
    /// Convert from a marked type
    pub fn from_marked(ctxt: &Context, item: &MarkedType) -> Result<Self> {
        let parsed = match item {
            MarkedType::Enum(item) => {
                let ItemEnum {
                    attrs: _,
                    vis: _,
                    enum_token: _,
                    ident: _, // handled in context
                    generics,
                    brace_token: _,
                    variants,
                } = item;

                // marked type should not have generics
                bail_if_exists!(&generics.lt_token);
                bail_if_exists!(&generics.gt_token);

                // build from variants
                Self::Algebraic(TypeADT::from_variants(ctxt, variants.iter())?)
            }
            MarkedType::Struct(item) => {
                let ItemStruct {
                    attrs: _,
                    vis: _,
                    struct_token: _,
                    ident: _, // handled in context
                    generics,
                    fields,
                    semi_token,
                } = item;

                // marked type should not have generics
                bail_if_exists!(&generics.lt_token);
                bail_if_exists!(&generics.gt_token);

                // exploit the similarity with ADT variant
                match ADTVariant::from_fields(ctxt, fields)? {
                    ADTVariant::Unit => bail_on!(fields, "unexpected unit type"),
                    ADTVariant::Tuple(tuple) => {
                        bail_if_missing!(semi_token, item, "expect ; at the end");
                        Self::Tuple(tuple)
                    }
                    ADTVariant::Record(record) => {
                        bail_if_exists!(semi_token);
                        Self::Record(record)
                    }
                }
            }
        };
        Ok(parsed)
    }
}

/// Represents all types that can appear in expressions
pub enum TypeUse {
    /// boolean
    Base(TypeTag),
    /// reference
    Ref(TypeTag),
}

impl TypeUse {
    /// Convert from a type
    pub fn from_type<T: CtxtForType>(ctxt: &T, ty: &Type) -> Result<Self> {
        let converted = match ty {
            Type::Reference(TypeReference {
                and_token: _,
                lifetime,
                mutability,
                elem,
            }) => {
                bail_if_exists!(lifetime);
                bail_if_exists!(mutability);
                Self::Ref(TypeTag::from_type(ctxt, elem)?)
            }
            Type::Path(TypePath { qself, path }) => {
                bail_if_exists!(qself);
                Self::Base(TypeTag::from_path(ctxt, path)?)
            }
            _ => bail_on!(ty, "expect type path"),
        };
        Ok(converted)
    }
}
