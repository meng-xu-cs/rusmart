use std::fmt::{Display, Formatter};

use syn::{Ident, Result};

use crate::parser::dsl::SysMacroName;
use crate::parser::err::bail_on;
use crate::parser::func::SysFuncName;
use crate::parser::generics::SysTrait;
use crate::parser::ty::SysTypeName;

/// Mark that this is a reserved identifier
pub trait ReservedIdent: Sized {
    /// try to parse from an identifier
    fn from_str(ident: &str) -> Option<Self>;
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
    /// Identifier for a type parameter
    TypeParamName
}

name! {
    /// Identifier for a user-defined type (i.e., non-reserved)
    UsrTypeName
}

name! {
    /// Identifier for a user-defined function (i.e., non-reserved)
    UsrFuncName
}

name! {
    /// Identifier for a variable
    VarName
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
            "not" | "and" | "or" | "xor"
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
            | "insert" | "contains"
            // map
            | "put_unchecked" | "get_unchecked" | "contains_key"
            // done
            => Self { ident: name.to_string() },
            // all other names are invalid
            _ => panic!("not an intrinsic function: {}", name),
        }
    }
}
