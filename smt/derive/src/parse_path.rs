use std::fmt::{Display, Formatter};

use syn::{Error, Ident, Pat, PatIdent, PathArguments, PathSegment, Result};

use crate::err::{bail_if_exists, bail_on};

/// Test whether an identifier is a reserved keyword
fn validate_identifier(ident: &Ident) -> Result<String> {
    let name = ident.to_string();
    match name.as_str() {
        "Boolean" | "Integer" | "Rational" | "Text" | "Box" | "Seq" | "Set" | "Map" | "Error" => {
            bail_on!(ident, "reserved type name");
        }
        "forall" | "exists" => {
            bail_on!(ident, "reserved function name");
        }
        "_" => {
            bail_on!(ident, "underscore not allowed");
        }
        _ => Ok(name),
    }
}

/// Identifier for a user-defined type (i.e., non-reserved)
#[derive(Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct TypeName {
    ident: String,
}

impl TryFrom<&Ident> for TypeName {
    type Error = Error;

    fn try_from(value: &Ident) -> Result<Self> {
        validate_identifier(value).map(|ident| Self { ident })
    }
}

impl AsRef<str> for TypeName {
    fn as_ref(&self) -> &str {
        &self.ident
    }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

/// Identifier for a user-defined function (i.e., non-reserved)
#[derive(Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct FuncName {
    ident: String,
}

impl AsRef<str> for FuncName {
    fn as_ref(&self) -> &str {
        &self.ident
    }
}

impl Display for FuncName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl TryFrom<&Ident> for FuncName {
    type Error = Error;

    fn try_from(value: &Ident) -> Result<Self> {
        validate_identifier(value).map(|ident| Self { ident })
    }
}

/// Identifier for a user-defined variable (i.e., non-reserved)
#[derive(Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct VarName {
    ident: String,
}

impl AsRef<str> for VarName {
    fn as_ref(&self) -> &str {
        &self.ident
    }
}

impl Display for VarName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl TryFrom<&Ident> for VarName {
    type Error = Error;

    fn try_from(value: &Ident) -> Result<Self> {
        validate_identifier(value).map(|ident| Self { ident })
    }
}

impl VarName {
    pub fn from_pat(pat: &Pat) -> Result<Self> {
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

                // just convert the ident
                ident.try_into()
            }
            _ => bail_on!(pat, "not an ident"),
        }
    }
}

/// Represents a namespace in the language
pub struct Namespace;

impl Namespace {
    pub fn consume_prefix<'a, I: Iterator<Item = &'a PathSegment>>(
        mut iter: I,
        expected: &[&'static str],
    ) -> Result<()> {
        let mut toks = expected.iter();
        loop {
            match (iter.next(), toks.next()) {
                (None, _) => return Ok(()),
                (Some(segment), None) => bail_on!(segment, "unexpected segment"),
                (Some(segment), Some(token)) => {
                    let PathSegment { ident, arguments } = segment;
                    if ident.to_string().as_str() != *token {
                        bail_on!(ident, "unknown path");
                    }
                    if !matches!(arguments, PathArguments::None) {
                        bail_on!(arguments, "unexpected path arguments");
                    }
                }
            }
        }
    }
}
