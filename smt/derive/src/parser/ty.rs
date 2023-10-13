use std::collections::BTreeMap;

use syn::{
    AngleBracketedGenericArguments, Field, Fields, FieldsNamed, FieldsUnnamed, GenericArgument,
    Ident, ItemEnum, ItemStruct, Path, PathArguments, PathSegment, Result, Type, TypePath, Variant,
};

use crate::parser::ctxt::MarkedType;
use crate::parser::err::{bail_if_empty, bail_if_exists, bail_if_missing, bail_on};
use crate::parser::generics::Generics;
use crate::parser::name::{ReservedIdent, TypeParamName, UsrTypeName};

/// A context suitable for type analysis
pub trait CtxtForType {
    /// Retrieve the generics declared (if any)
    fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics>;
}

/// Reserved type name
pub enum SysTypeName {
    Boolean,
    Integer,
    Rational,
    Text,
    Cloak,
    Seq,
    Set,
    Map,
    Error,
}

impl ReservedIdent for SysTypeName {
    fn from_str(ident: &str) -> Option<Self> {
        let matched = match ident.to_string().as_str() {
            "Boolean" => Self::Boolean,
            "Integer" => Self::Integer,
            "Rational" => Self::Rational,
            "Text" => Self::Text,
            "Cloak" => Self::Cloak,
            "Seq" => Self::Seq,
            "Set" => Self::Set,
            "Map" => Self::Map,
            "Error" => Self::Error,
            _ => return None,
        };
        Some(matched)
    }
}

/// A type name
pub enum TypeName {
    Sys(SysTypeName),
    Usr(UsrTypeName),
    Param(TypeParamName),
}

impl TypeName {
    /// Try to convert an ident into a type name
    fn try_from(generics: &Generics, ident: &Ident) -> Result<Self> {
        let name = ident.to_string();
        let parsed = match SysTypeName::from_str(&name) {
            None => {
                // type parameters take priority over user-defined type names
                let param_name = ident.try_into()?;
                if generics.get(&param_name).is_some() {
                    Self::Param(param_name)
                } else {
                    Self::Usr(ident.try_into()?)
                }
            }
            Some(n) => Self::Sys(n),
        };
        Ok(parsed)
    }
}

/// A parser for type
struct TypeParser<'a, CTX: CtxtForType> {
    /// context provider
    ctxt: &'a CTX,
    generics: &'a Generics,
}

/// A unique and complete reference to an SMT-related type
#[derive(Clone, Eq, PartialEq)]
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
    Cloak(Box<TypeTag>),
    /// SMT-sequence
    Seq(Box<TypeTag>),
    /// SMT-set
    Set(Box<TypeTag>),
    /// SMT-array
    Map(Box<TypeTag>, Box<TypeTag>),
    /// dynamic error type
    Error,
    /// user-defined type
    User(UsrTypeName, Vec<TypeTag>),
    /// parameter
    Parameter(TypeParamName),
}

impl TypeTag {
    /// Convert from a type argument pack
    fn from_args<CTX: CtxtForType>(
        parser: &TypeParser<CTX>,
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
                    list.push(Self::from_type(parser, ty)?);
                }
                _ => bail_on!(arg, "invalid type argument"),
            }
        }
        Ok(list)
    }

    /// Convert from a type argument pack, expecting 1 argument
    fn from_args_expect_1<CTX: CtxtForType>(
        parser: &TypeParser<CTX>,
        pack: &AngleBracketedGenericArguments,
    ) -> Result<Self> {
        let mut iter = Self::from_args(parser, pack)?.into_iter();
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
    fn from_args_expect_2<CTX: CtxtForType>(
        parser: &TypeParser<CTX>,
        pack: &AngleBracketedGenericArguments,
    ) -> Result<(Self, Self)> {
        let mut iter = Self::from_args(parser, pack)?.into_iter();
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
    fn from_path<CTX: CtxtForType>(parser: &TypeParser<CTX>, path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        // in reverse order
        let mut iter = segments.iter().rev();
        let segment = bail_if_missing!(iter.next(), path, "ident");
        let PathSegment { ident, arguments } = segment;

        let tag = match TypeName::try_from(parser.generics, ident)? {
            TypeName::Sys(intrinsic) => match (intrinsic, arguments) {
                (SysTypeName::Boolean, PathArguments::None) => Self::Boolean,
                (SysTypeName::Integer, PathArguments::None) => Self::Integer,
                (SysTypeName::Rational, PathArguments::None) => Self::Rational,
                (SysTypeName::Text, PathArguments::None) => Self::Text,
                (SysTypeName::Cloak, PathArguments::AngleBracketed(pack)) => {
                    let sub = Self::from_args_expect_1(parser, pack)?;
                    Self::Cloak(sub.into())
                }
                (SysTypeName::Seq, PathArguments::AngleBracketed(pack)) => {
                    let sub = Self::from_args_expect_1(parser, pack)?;
                    Self::Seq(sub.into())
                }
                (SysTypeName::Set, PathArguments::AngleBracketed(pack)) => {
                    let sub = Self::from_args_expect_1(parser, pack)?;
                    Self::Set(sub.into())
                }
                (SysTypeName::Map, PathArguments::AngleBracketed(pack)) => {
                    let (key, val) = Self::from_args_expect_2(parser, pack)?;
                    Self::Map(key.into(), val.into())
                }
                (SysTypeName::Error, PathArguments::None) => Self::Error,
                _ => bail_on!(segment, "invalid type tag for intrinsic type"),
            },
            TypeName::Usr(name) => match parser.ctxt.get_type_generics(&name) {
                None => bail_on!(ident, "no such type"),
                Some(generics) => match generics.len() {
                    0 => {
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected");
                        }
                        Self::User(name, vec![])
                    }
                    n => match arguments {
                        PathArguments::None => bail_on!(ident, "expect type arguments"),
                        PathArguments::AngleBracketed(pack) => {
                            let args = Self::from_args(parser, pack)?;
                            if args.len() != n {
                                bail_on!(arguments, "type argument number mismatch");
                            }
                            Self::User(name, args)
                        }
                        _ => bail_on!(arguments, "invalid type arguments"),
                    },
                },
            },
            TypeName::Param(name) => Self::Parameter(name),
        };
        bail_if_exists!(iter.next());

        Ok(tag)
    }

    /// Convert from a type
    fn from_type<CTX: CtxtForType>(parser: &TypeParser<CTX>, ty: &Type) -> Result<Self> {
        match ty {
            Type::Path(TypePath { qself, path }) => {
                bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
                Self::from_path(parser, path)
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
    fn from_fields<'a, I: Iterator<Item = &'a Field>, CTX: CtxtForType>(
        parser: &TypeParser<CTX>,
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

            let tag = TypeTag::from_type(parser, ty)?;
            slots.push(tag);
        }

        Ok(Self { slots })
    }

    /// Get slots
    pub fn slots(&self) -> &[TypeTag] {
        &self.slots
    }
}

/// Represents a record definition
pub struct TypeRecord {
    fields: BTreeMap<String, TypeTag>,
}

impl TypeRecord {
    /// Convert from a fields token
    fn from_fields<'b, I: Iterator<Item = &'b Field>, CTX: CtxtForType>(
        parser: &TypeParser<CTX>,
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

            let tag = TypeTag::from_type(parser, ty)?;
            match fields.insert(name.to_string(), tag) {
                None => (),
                Some(_) => bail_on!(ident, "duplicated field name"),
            }
        }

        Ok(Self { fields })
    }

    /// Get fields
    pub fn fields(&self) -> &BTreeMap<String, TypeTag> {
        &self.fields
    }
}

/// A helper enum to represent a variant definition in an ADT type
pub enum EnumVariant {
    Unit,
    Tuple(TypeTuple),
    Record(TypeRecord),
}

impl EnumVariant {
    /// Convert from a fields token
    fn from_fields<CTX: CtxtForType>(parser: &TypeParser<CTX>, fields: &Fields) -> Result<Self> {
        let variant = match fields {
            Fields::Unit => Self::Unit,
            Fields::Named(FieldsNamed {
                brace_token: _,
                named,
            }) => Self::Record(TypeRecord::from_fields(parser, named.iter())?),
            Fields::Unnamed(FieldsUnnamed {
                paren_token: _,
                unnamed,
            }) => Self::Tuple(TypeTuple::from_fields(parser, unnamed.iter())?),
        };
        Ok(variant)
    }
}

/// Represents an ADT definition
pub struct TypeEnum {
    variants: BTreeMap<String, EnumVariant>,
}

impl TypeEnum {
    /// Convert from a list of variants
    fn from_variants<'a, I: Iterator<Item = &'a Variant>, CTX: CtxtForType>(
        parser: &TypeParser<CTX>,
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
            let branch = EnumVariant::from_fields(parser, fields)?;
            match variants.insert(ident.to_string(), branch) {
                None => (),
                Some(_) => bail_on!(ident, "duplicated variant name"),
            }
        }

        Ok(Self { variants })
    }

    /// Get variants as map
    pub fn variants(&self) -> &BTreeMap<String, EnumVariant> {
        &self.variants
    }
}

/// The body part of a type definition
pub enum TypeBody {
    Tuple(TypeTuple),
    Record(TypeRecord),
    Enum(TypeEnum),
}

impl TypeBody {
    /// Convert from a marked type
    pub fn from_marked<CTX: CtxtForType>(
        ctxt: &CTX,
        generics: &Generics,
        item: &MarkedType,
    ) -> Result<Self> {
        let parser = TypeParser { ctxt, generics };

        let parsed = match item {
            MarkedType::Enum(item) => {
                let ItemEnum {
                    attrs: _,
                    vis: _,
                    enum_token: _,
                    ident: _,    // handled earlier
                    generics: _, // handled earlier
                    brace_token: _,
                    variants,
                } = item;
                bail_if_empty!(variants, "variants");

                // build from variants
                Self::Enum(TypeEnum::from_variants(&parser, variants.iter())?)
            }
            MarkedType::Struct(item) => {
                let ItemStruct {
                    attrs: _,
                    vis: _,
                    struct_token: _,
                    ident: _,    // handled earlier
                    generics: _, // handled earlier
                    fields,
                    semi_token,
                } = item;

                // exploit the similarity with ADT variant
                match EnumVariant::from_fields(&parser, fields)? {
                    EnumVariant::Unit => bail_on!(item, "expect fields or slots"),
                    EnumVariant::Tuple(tuple) => {
                        if tuple.slots.is_empty() {
                            bail_on!(fields, "expect slots");
                        }
                        bail_if_missing!(semi_token, item, "expect ; at the end");
                        Self::Tuple(tuple)
                    }
                    EnumVariant::Record(record) => {
                        if record.fields.is_empty() {
                            bail_on!(fields, "expect fields");
                        }
                        bail_if_exists!(semi_token);
                        Self::Record(record)
                    }
                }
            }
        };
        Ok(parsed)
    }

    /// Merge with the generics to create a type def
    pub fn combine(self, generics: Generics) -> TypeDef {
        TypeDef {
            head: generics,
            body: self,
        }
    }
}

/// A complete definition of a type (generics + body)
pub struct TypeDef {
    head: Generics,
    body: TypeBody,
}

#[cfg(test)]
mod tests {
    use crate::parser::test::unit_test;

    unit_test!(
        struct_no_field,
        {
            #[smt_type]
            struct S;
        },
        "expect fields or slots"
    );

    unit_test!(
        tuple_no_slot,
        {
            #[smt_type]
            struct S();
        },
        "expect slots"
    );

    unit_test!(
        record_no_field,
        {
            #[smt_type]
            struct S {}
        },
        "expect fields"
    );

    unit_test!(
        enum_no_variant,
        {
            #[smt_type]
            enum S {}
        },
        "expect variants"
    );

    unit_test!(
        no_user_type,
        {
            #[smt_type]
            struct S(A);
        },
        "no such type"
    );
}
