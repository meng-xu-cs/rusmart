use std::fmt::{Display, Formatter};

use syn::{Ident, Result};

use crate::parser::err::bail_on;
use crate::parser::generics::SysTrait;

/// Test whether an identifier is a reserved keyword
fn validate_user_ident(ident: &Ident) -> Result<String> {
    // check for reserved keywords
    if SysTrait::from_ident(ident).is_some() {
        bail_on!(ident, "reserved trait name");
    }

    // TODO: check the rest

    let name = ident.to_string();
    match name.as_str() {
        "Boolean" | "Integer" | "Rational" | "Text" | "Cloak" | "Seq" | "Set" | "Map" | "Error" => {
            bail_on!(ident, "reserved type name");
        }
        "forall" | "exists" => {
            bail_on!(ident, "reserved macro name");
        }
        "into" => {
            bail_on!(ident, "reserved method name");
        }
        "_" => {
            bail_on!(ident, "underscore not allowed");
        }
        _ => Ok(name),
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
    };
}

name! {
    /// Identifier for a user-defined type parameter
    TypeParamName
}

name! {
    /// Identifier for a user-defined type (i.e., non-reserved)
    TypeName
}

name! {
    /// Identifier for a user-defined function (i.e., non-reserved)
    FuncName
}

/// Mark that this is a reserved identifier
pub trait ReservedIdent: Sized {
    /// try to parse from an identifier
    fn from_ident(ident: &Ident) -> Option<Self>;
}
