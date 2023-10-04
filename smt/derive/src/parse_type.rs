use std::collections::BTreeMap;

use syn::{
    AngleBracketedGenericArguments, Field, Fields, GenericArgument, ItemStruct, Path,
    PathArguments, PathSegment, Result, Type, TypePath,
};

use crate::parse_ctxt::{bail_if_exists, bail_if_missing, bail_on, Context, MarkedType, TypeName};

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
    fn from_args(ctxt: &Context, pack: &AngleBracketedGenericArguments) -> Result<Vec<Self>> {
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
    fn from_args_expect_1(ctxt: &Context, pack: &AngleBracketedGenericArguments) -> Result<Self> {
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
    fn from_args_expect_2(
        ctxt: &Context,
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
    fn from_path(ctxt: &Context, path: &Path) -> Result<Self> {
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
                        if ctxt.get_type(&name).is_none() {
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
    pub fn from_type(ctxt: &Context, ty: &Type) -> Result<Self> {
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

/// Represents a record definition
pub struct TypeRecord {
    fields: BTreeMap<String, TypeTag>,
}

impl TypeRecord {
    /// Convert from an iterator of fields
    pub fn from_fields(ctxt: &Context, fields: &Fields) -> Result<Self> {
        let mut decls = BTreeMap::new();
        for field in fields {
            let Field {
                attrs: _,
                vis: _,
                mutability: _,
                ident,
                colon_token,
                ty,
            } = field;

            bail_if_missing!(colon_token, field, "colon");
            let name = bail_if_missing!(ident, field, "name");
            let tag = TypeTag::from_type(ctxt, ty)?;

            match decls.insert(name.to_string(), tag) {
                None => (),
                Some(_) => bail_on!(ident, "duplicated field name"),
            }
        }
        Ok(Self { fields: decls })
    }
}

/// A helper enum to represent a variant definition in an ADT type
pub enum ADTVariant {
    Unit,
    Tuple(TypeTuple),
    Record(TypeRecord),
}

/// Represents an ADT definition
pub struct TypeADT {
    variants: BTreeMap<String, ADTVariant>,
}

/// A complete definition of type
pub enum TypeBody {
    Record(TypeRecord),
    Algebraic(TypeADT),
}

pub fn parse_type(ctxt: &Context, item: &MarkedType) {}
