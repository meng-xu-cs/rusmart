use syn::{Ident, Result};

use crate::parser::err::bail_on;

/// Test whether an identifier is a reserved keyword
fn validate_user_ident(ident: &Ident) -> Result<String> {
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

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.ident)
            }
        }

        impl TryFrom<&syn::Ident> for $name {
            type Error = syn::Error;

            fn try_from(value: &syn::Ident) -> syn::Result<Self> {
                validate_user_ident(value).map(|ident| Self { ident })
            }
        }
    };
}

name! {
    /// Identifier for a user-defined type (i.e., non-reserved)
    TypeName
}

name! {
    /// Identifier for a user-defined function (i.e., non-reserved)
    FuncName
}
