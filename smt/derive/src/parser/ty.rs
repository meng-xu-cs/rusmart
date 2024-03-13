use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter};

use itertools::Itertools;
use syn::{
    AngleBracketedGenericArguments, Field, FieldMutability, Fields, FieldsNamed, FieldsUnnamed,
    GenericArgument, Ident, ItemEnum, ItemStruct, Path, PathArguments, PathSegment, Result, Type,
    TypePath, TypeTuple as TypePack, Variant,
};

use crate::parser::ctxt::{ContextWithGenerics, MarkedType};
use crate::parser::err::{bail_if_empty, bail_if_exists, bail_if_missing, bail_on};
use crate::parser::generics::Generics;
use crate::parser::name::{ReservedIdent, TypeParamName, UsrTypeName};

/// A context suitable for type analysis
pub trait CtxtForType {
    /// Retrieve the generics in the current context
    fn generics(&self) -> &Generics;

    /// Retrieve the generics declared (if any) for a user-defined type
    fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics>;
}

/// A context provider for type parsing
struct TypeParseCtxt<'a> {
    ctxt: &'a ContextWithGenerics,
    generics: &'a Generics,
}

impl CtxtForType for TypeParseCtxt<'_> {
    fn generics(&self) -> &Generics {
        self.generics
    }

    fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics> {
        self.ctxt.get_type_generics(name)
    }
}

/// Reserved type name
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
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

impl Display for SysTypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::Boolean => "Boolean",
            Self::Integer => "Integer",
            Self::Rational => "Rational",
            Self::Text => "Text",
            Self::Cloak => "Cloak",
            Self::Seq => "Seq",
            Self::Set => "Set",
            Self::Map => "Map",
            Self::Error => "Error",
        };
        f.write_str(name)
    }
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

impl SysTypeName {
    /// Create the associated generics for an intrinsic type
    pub fn generics(&self) -> Generics {
        match self {
            Self::Boolean | Self::Integer | Self::Rational | Self::Text | Self::Error => {
                Generics::intrinsic(vec![])
            }
            Self::Cloak | Self::Seq | Self::Set => {
                Generics::intrinsic(vec![TypeParamName::intrinsic("T")])
            }
            Self::Map => Generics::intrinsic(vec![
                TypeParamName::intrinsic("K"),
                TypeParamName::intrinsic("V"),
            ]),
        }
    }

    /// Convert an intrinsic type to a type tag
    pub fn as_type_tag(&self) -> TypeTag {
        let t = || Box::new(TypeTag::Parameter(TypeParamName::intrinsic("T")));
        let k = || Box::new(TypeTag::Parameter(TypeParamName::intrinsic("K")));
        let v = || Box::new(TypeTag::Parameter(TypeParamName::intrinsic("V")));

        match self {
            Self::Boolean => TypeTag::Boolean,
            Self::Integer => TypeTag::Integer,
            Self::Rational => TypeTag::Rational,
            Self::Text => TypeTag::Text,
            Self::Cloak => TypeTag::Cloak(t()),
            Self::Seq => TypeTag::Seq(t()),
            Self::Set => TypeTag::Set(t()),
            Self::Map => TypeTag::Map(k(), v()),
            Self::Error => TypeTag::Error,
        }
    }
}

/// A type name
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum TypeName {
    Sys(SysTypeName),
    Usr(UsrTypeName),
    Param(TypeParamName),
}

impl TypeName {
    /// Try to convert an ident into a type name
    pub fn try_from(generics: &Generics, ident: &Ident) -> Result<Self> {
        let name = ident.to_string();
        let parsed = match SysTypeName::from_str(&name) {
            None => {
                // type parameters take priority over user-defined type names
                let param_name = ident.try_into()?;
                if generics.params.contains(&param_name) {
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

impl Display for TypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sys(name) => name.fmt(f),
            Self::Usr(name) => name.fmt(f),
            Self::Param(name) => name.fmt(f),
        }
    }
}

/// A unique and complete reference to an SMT-related type
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
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
    /// a tuple of types
    Pack(Vec<TypeTag>),
    /// parameter
    Parameter(TypeParamName),
}

impl TypeTag {
    /// Convert from a type argument pack
    pub fn from_args<CTX: CtxtForType>(
        ctxt: &CTX,
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
    fn from_args_expect_1<CTX: CtxtForType>(
        ctxt: &CTX,
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
    fn from_args_expect_2<CTX: CtxtForType>(
        ctxt: &CTX,
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
    fn from_path<CTX: CtxtForType>(ctxt: &CTX, path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        // in reverse order
        let mut iter = segments.iter().rev();
        let segment = bail_if_missing!(iter.next(), path, "ident");
        let PathSegment { ident, arguments } = segment;

        let tag = match TypeName::try_from(ctxt.generics(), ident)? {
            TypeName::Sys(intrinsic) => match (intrinsic, arguments) {
                (SysTypeName::Boolean, PathArguments::None) => Self::Boolean,
                (SysTypeName::Integer, PathArguments::None) => Self::Integer,
                (SysTypeName::Rational, PathArguments::None) => Self::Rational,
                (SysTypeName::Text, PathArguments::None) => Self::Text,
                (SysTypeName::Cloak, PathArguments::AngleBracketed(pack)) => {
                    let sub = Self::from_args_expect_1(ctxt, pack)?;
                    Self::Cloak(sub.into())
                }
                (SysTypeName::Seq, PathArguments::AngleBracketed(pack)) => {
                    let sub = Self::from_args_expect_1(ctxt, pack)?;
                    Self::Seq(sub.into())
                }
                (SysTypeName::Set, PathArguments::AngleBracketed(pack)) => {
                    let sub = Self::from_args_expect_1(ctxt, pack)?;
                    Self::Set(sub.into())
                }
                (SysTypeName::Map, PathArguments::AngleBracketed(pack)) => {
                    let (key, val) = Self::from_args_expect_2(ctxt, pack)?;
                    Self::Map(key.into(), val.into())
                }
                (SysTypeName::Error, PathArguments::None) => Self::Error,
                _ => bail_on!(segment, "invalid type tag for intrinsic type"),
            },
            TypeName::Usr(name) => match ctxt.get_type_generics(&name) {
                None => bail_on!(ident, "no such type"),
                Some(generics) => match generics.params.len() {
                    0 => {
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected");
                        }
                        Self::User(name, vec![])
                    }
                    n => match arguments {
                        PathArguments::None => bail_on!(ident, "expect type arguments"),
                        PathArguments::AngleBracketed(pack) => {
                            let args = Self::from_args(ctxt, pack)?;
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
    pub fn from_type<CTX: CtxtForType>(ctxt: &CTX, ty: &Type) -> Result<Self> {
        match ty {
            Type::Path(TypePath { qself, path }) => {
                bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
                Self::from_path(ctxt, path)
            }
            Type::Tuple(TypePack {
                paren_token: _,
                elems,
            }) => {
                let mut pack = vec![];
                for elem in elems {
                    pack.push(Self::from_type(ctxt, elem)?);
                }
                Ok(Self::Pack(pack))
            }
            _ => bail_on!(ty, "expect type path or tuple"),
        }
    }

    /// Convert a series of generic arguments
    pub fn from_generics<CTX: CtxtForType>(
        ctxt: &CTX,
        generics: &AngleBracketedGenericArguments,
    ) -> Result<Vec<Self>> {
        let AngleBracketedGenericArguments {
            colon2_token,
            lt_token: _,
            args,
            gt_token: _,
        } = generics;
        bail_if_missing!(colon2_token, generics, "::");
        bail_if_empty!(args, generics, "type argument");

        let mut arguments = vec![];
        for item in args {
            match item {
                GenericArgument::Type(ty_arg) => {
                    let t = Self::from_type(ctxt, ty_arg)?;
                    arguments.push(t);
                }
                _ => bail_on!(item, "not a type argument"),
            }
        }
        Ok(arguments)
    }

    /// Collect type parameters involved in this type tag (recursive function)
    fn type_params_used_recursive(&self, params: &mut BTreeSet<TypeParamName>) {
        match self {
            Self::Boolean | Self::Integer | Self::Rational | Self::Text | Self::Error => (),
            Self::Cloak(sub) => sub.type_params_used_recursive(params),
            Self::Seq(sub) => sub.type_params_used_recursive(params),
            Self::Set(sub) => sub.type_params_used_recursive(params),
            Self::Map(key, val) => {
                key.type_params_used_recursive(params);
                val.type_params_used_recursive(params);
            }
            Self::User(_, args) => {
                for arg in args {
                    arg.type_params_used_recursive(params);
                }
            }
            Self::Pack(elems) => {
                for elem in elems {
                    elem.type_params_used_recursive(params);
                }
            }
            Self::Parameter(name) => {
                params.insert(name.clone());
            }
        }
    }

    /// Collect type parameters involved in this type tag
    pub fn type_params_used(&self) -> BTreeSet<TypeParamName> {
        let mut params = BTreeSet::new();
        self.type_params_used_recursive(&mut params);
        params
    }
}

impl Display for TypeTag {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean => write!(f, "Boolean"),
            Self::Integer => write!(f, "Integer"),
            Self::Rational => write!(f, "Rational"),
            Self::Text => write!(f, "Text"),
            Self::Cloak(sub) => write!(f, "Cloak<{}>", sub),
            Self::Seq(sub) => write!(f, "Seq<{}>", sub),
            Self::Set(sub) => write!(f, "Set<{}>", sub),
            Self::Map(key, val) => write!(f, "Map<{},{}>", key, val),
            Self::Error => write!(f, "Error"),
            Self::User(name, args) => {
                if args.is_empty() {
                    name.fmt(f)
                } else {
                    write!(f, "{}<{}>", name, args.iter().format(","))
                }
            }
            Self::Pack(elems) => {
                write!(f, "({})", elems.iter().format(","))
            }
            Self::Parameter(name) => name.fmt(f),
        }
    }
}

/// Represents a tuple definition
pub struct TypeTuple {
    pub slots: Vec<TypeTag>,
}

impl TypeTuple {
    /// Convert from a list of fields
    fn from_fields<'a, I: Iterator<Item = &'a Field>, CTX: CtxtForType>(
        ctxt: &CTX,
        items: I,
    ) -> Result<Self> {
        let mut slots = vec![];

        for field in items {
            let Field {
                attrs: _,
                vis: _,
                mutability,
                ident,
                colon_token,
                ty,
            } = field;

            if !matches!(mutability, FieldMutability::None) {
                bail_on!(field, "unexpected slot mutability");
            }
            bail_if_exists!(ident);
            bail_if_exists!(colon_token);

            let tag = TypeTag::from_type(ctxt, ty)?;
            slots.push(tag);
        }

        Ok(Self { slots })
    }
}

impl Display for TypeTuple {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.slots.iter().format(","))
    }
}

/// Represents a record definition
pub struct TypeRecord {
    pub fields: BTreeMap<String, TypeTag>,
}

impl TypeRecord {
    /// Convert from a fields token
    fn from_fields<'b, I: Iterator<Item = &'b Field>, CTX: CtxtForType>(
        ctxt: &CTX,
        items: I,
    ) -> Result<Self> {
        let mut fields = BTreeMap::new();

        for field in items {
            let Field {
                attrs: _,
                vis: _,
                mutability,
                ident,
                colon_token,
                ty,
            } = field;

            if !matches!(mutability, FieldMutability::None) {
                bail_on!(field, "unexpected field mutability");
            }
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

impl Display for TypeRecord {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.fields
                .iter()
                .format_with(",", |(n, t), p| p(&format_args!("{}:{}", n, t))),
        )
    }
}

/// A helper enum to represent a variant definition in an ADT type
pub enum EnumVariant {
    Unit,
    Tuple(TypeTuple),
    Record(TypeRecord),
}

impl EnumVariant {
    /// Convert from fields token
    fn from_fields<CTX: CtxtForType>(ctxt: &CTX, fields: &Fields) -> Result<Self> {
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

impl Display for EnumVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => f.write_str(""),
            Self::Tuple(details) => details.fmt(f),
            Self::Record(details) => details.fmt(f),
        }
    }
}

/// Represents an ADT definition
pub struct TypeEnum {
    pub variants: BTreeMap<String, EnumVariant>,
}

impl TypeEnum {
    /// Convert from a list of variants
    fn from_variants<'a, I: Iterator<Item = &'a Variant>, CTX: CtxtForType>(
        ctxt: &CTX,
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
            let branch = EnumVariant::from_fields(ctxt, fields)?;

            // check variant consistency
            match &branch {
                EnumVariant::Unit => (),
                EnumVariant::Tuple(tuple) => bail_if_empty!(tuple.slots, variant, "slots"),
                EnumVariant::Record(record) => bail_if_empty!(record.fields, variant, "fields"),
            }
            match variants.insert(ident.to_string(), branch) {
                None => (),
                Some(_) => bail_on!(ident, "duplicated variant name"),
            }
        }

        Ok(Self { variants })
    }
}

impl Display for TypeEnum {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.variants
                .iter()
                .format_with("\n", |(n, t), p| p(&format_args!("{}{}", n, t))),
        )
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
    pub fn from_marked(
        driver: &ContextWithGenerics,
        generics: &Generics,
        item: &MarkedType,
    ) -> Result<Self> {
        let ctxt = TypeParseCtxt {
            ctxt: driver,
            generics,
        };
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
                bail_if_empty!(variants, item, "variants");

                // build from variants
                Self::Enum(TypeEnum::from_variants(&ctxt, variants.iter())?)
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
                match EnumVariant::from_fields(&ctxt, fields)? {
                    EnumVariant::Unit => bail_on!(item, "expect fields or slots"),
                    EnumVariant::Tuple(tuple) => {
                        bail_if_empty!(tuple.slots, item, "slots");
                        bail_if_missing!(semi_token, item, "expect ; at the end");
                        Self::Tuple(tuple)
                    }
                    EnumVariant::Record(record) => {
                        bail_if_empty!(record.fields, item, "fields");
                        bail_if_exists!(semi_token);
                        Self::Record(record)
                    }
                }
            }
        };
        Ok(parsed)
    }
}

impl Display for TypeBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tuple(details) => details.fmt(f),
            Self::Record(details) => details.fmt(f),
            Self::Enum(details) => details.fmt(f),
        }
    }
}

/// A complete definition of a type (generics + body)
pub struct TypeDef {
    pub head: Generics,
    pub body: TypeBody,
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.head, self.body)
    }
}
