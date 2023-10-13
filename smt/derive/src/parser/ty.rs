use syn::{
    AngleBracketedGenericArguments, GenericArgument, Path, PathArguments, PathSegment, Result,
    Type, TypePath,
};

use crate::parser::err::{bail_if_exists, bail_on};
use crate::parser::generics::Generics;
use crate::parser::name::{TypeParamName, UsrTypeName};

/// A context suitable for type analysis
pub trait CtxtForType {
    /// Retrieve the generics declared (if any)
    fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics>;
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
        let tag = match iter.next() {
            None => bail_on!(segments, "invalid path"),
            Some(segment) => {
                let PathSegment { ident, arguments } = segment;
                match (ident.to_string().as_str(), arguments) {
                    ("Boolean", PathArguments::None) => Self::Boolean,
                    ("Integer", PathArguments::None) => Self::Integer,
                    ("Rational", PathArguments::None) => Self::Rational,
                    ("Text", PathArguments::None) => Self::Text,
                    ("Cloak", PathArguments::AngleBracketed(pack)) => {
                        let sub = Self::from_args_expect_1(ctxt, pack)?;
                        Self::Cloak(sub.into())
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
                        match ctxt.get_type_generics(&name) {
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
                                        let args = Self::from_args(ctxt, pack)?;
                                        if args.len() != n {
                                            bail_on!(arguments, "type argument number mismatch");
                                        }
                                        Self::User(name, args)
                                    }
                                    _ => bail_on!(arguments, "invalid type arguments"),
                                },
                            },
                        }
                    }
                }
            }
        };

        // disallow more items in the match
        match iter.next() {
            None => (),
            Some(segment) => bail_on!(segment, "unexpected segment"),
        }
        Ok(tag)
    }

    /// Convert from a type
    pub fn from_type<CTX: CtxtForType>(ctxt: &CTX, ty: &Type) -> Result<Self> {
        match ty {
            Type::Path(TypePath { qself, path }) => {
                bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
                Self::from_path(ctxt, path)
            }
            _ => bail_on!(ty, "expect type path"),
        }
    }
}
