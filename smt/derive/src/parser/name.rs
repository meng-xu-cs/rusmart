use std::fmt::{Display, Formatter};

use syn::{Ident, Pat, PatIdent, Path, PathArguments, PathSegment, Result};

use crate::parser::dsl::SysMacroName;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::func::SysFuncName;
use crate::parser::generics::SysTrait;
use crate::parser::ty::SysTypeName;

/// Mark that this is a reserved identifier
pub trait ReservedIdent: Sized {
    /// try to parse from an identifier
    fn from_str(ident: &str) -> Option<Self>;

    /// Expect a reserved identifier from the path
    fn parse_path(path: &Path) -> Result<Self> {
        let ident = parse_ident_from_path(path)?;
        match Self::from_str(ident.to_string().as_str()) {
            None => bail_on!(ident, "not a reserved identifier"),
            Some(v) => Ok(v),
        }
    }
}

/// Test whether an identifier is a reserved keyword
fn validate_user_ident(ident: &Ident) -> Result<String> {
    let name = ident.to_string();

    // check for reserved keywords
    if SysTrait::from_str(&name).is_some() {
        bail_on!(ident, "reserved trait name");
    }
    if SysTypeName::from_str(&name).is_some() {
        bail_on!(ident, "reserved type name");
    }
    if SysFuncName::from_str(&name).is_some() {
        bail_on!(ident, "reserved method name");
    }
    if SysMacroName::from_str(&name).is_some() {
        bail_on!(ident, "reserved macro name");
    }
    if name.as_str() == "_" {
        bail_on!(ident, "underscore not allowed");
    }

    Ok(name)
}

/// Parse a plain identifier from a path
fn parse_ident_from_path(path: &Path) -> Result<&Ident> {
    let Path {
        leading_colon,
        segments,
    } = path;
    bail_if_exists!(leading_colon);

    let mut iter = segments.iter().rev();
    let segment = bail_if_missing!(iter.next(), path, "invalid path");
    let PathSegment { ident, arguments } = segment;
    if !matches!(arguments, PathArguments::None) {
        bail_on!(arguments, "unexpected arguments");
    }
    bail_if_exists!(iter.next());

    Ok(ident)
}

/// Parse a plain identifier from a pattern
fn parse_ident_from_pat(pat: &Pat) -> Result<&Ident> {
    match pat {
        Pat::Ident(decl) => {
            let PatIdent {
                attrs: _,
                by_ref,
                mutability,
                ident,
                subpat,
            } = decl;

            // plain name only
            bail_if_exists!(by_ref);
            bail_if_exists!(mutability);
            bail_if_exists!(subpat.as_ref().map(|(_, sub)| sub));
            Ok(ident)
        }
        _ => bail_on!(pat, "not an ident"),
    }
}

/// Utility macro to define a name
macro_rules! name {
    ($(#[$meta:meta])* $name:ident) => {
        $(#[$meta])*
        #[derive(Ord, PartialOrd, Eq, PartialEq, Clone)]
        pub struct $name {
            ident: String,
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &str {
                &self.ident
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.ident)
            }
        }

        impl TryFrom<&Ident> for $name {
            type Error = syn::Error;

            fn try_from(value: &Ident) -> Result<Self> {
                validate_user_ident(value).map(|ident| Self { ident })
            }
        }

        impl TryFrom<&Path> for $name {
            type Error = syn::Error;

            fn try_from(value: &Path) -> Result<Self> {
                parse_ident_from_path(value)?.try_into()
            }
        }

        impl TryFrom<&Pat> for $name {
            type Error = syn::Error;

            fn try_from(value: &Pat) -> Result<Self> {
                parse_ident_from_pat(value)?.try_into()
            }
        }
    };
    ($(#[$meta:meta])* $name:ident > $child:ty) => {
        name!($(#[$meta])* $name);

        impl From<$child> for $name {
            fn from(name: $child) -> Self {
                Self {
                    ident: name.to_string(),
                }
            }
        }

        impl From<&$child> for $name {
            fn from(name: &$child) -> Self {
                Self {
                    ident: name.to_string(),
                }
            }
        }
    };
}

name! {
    /// Identifier for a type parameter
    TypeParamName
        > crate::ir::name::SmtSortName
}

name! {
    /// Identifier for a user-defined type (i.e., non-reserved)
    UsrTypeName
        > crate::ir::name::UsrSortName
}

name! {
    /// Identifier for a user-defined function (i.e., non-reserved)
    UsrFuncName
        > crate::ir::name::UsrFunName
}

name! {
    /// Identifier for an axiom
    AxiomName
        > crate::ir::name::UsrAxiomName
}

name! {
    /// Identifier for a variable
    VarName
        > crate::ir::name::Symbol
}

impl TypeParamName {
    /// Make a type parameter name for intrinsic usage
    pub fn intrinsic(name: &str) -> Self {
        match name {
            "T" | "K" | "V" => Self {
                ident: name.to_string(),
            },
            // all other names are invalid
            _ => panic!("not an intrinsic type parameter: {}", name),
        }
    }
}

impl UsrFuncName {
    /// Make a function name for intrinsic functions
    pub fn intrinsic(name: &str) -> Self {
        match name {
            // logical
            "not" | "and" | "or" | "xor" | "implies"
            // arithmetic
            | "add" | "sub" | "mul" | "div" | "rem"
            // comparison
            | "lt" | "le" | "ge" | "gt"
            // error
            | "fresh" | "merge"
            // cloak
            | "shield" | "reveal"
            // collections (common)
            | "empty" | "length"
            // seq
            | "append" | "at_unchecked" | "includes"
            // set
            | "insert" | "remove" | "contains"
            // map
            | "put_unchecked" | "get_unchecked" | "del_unchecked" | "contains_key"
            // done
            => Self { ident: name.to_string() },
            // all other names are invalid
            _ => panic!("not an intrinsic function: {}", name),
        }
    }
}
