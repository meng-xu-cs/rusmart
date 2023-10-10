use std::fmt::{Display, Formatter};

use syn::{
    Error, Expr as Exp, ExprPath, Ident, Pat, PatIdent, Path, PathArguments, PathSegment, Result,
};

use crate::err::{bail_if_exists, bail_on};

/// Test whether an identifier is a reserved keyword
fn validate_usr_ident(ident: &Ident) -> Result<String> {
    let name = ident.to_string();
    match name.as_str() {
        "Boolean" | "Integer" | "Rational" | "Text" | "Box" | "Seq" | "Set" | "Map" | "Error" => {
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

/// Identifier for a user-defined type (i.e., non-reserved)
#[derive(Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct TypeName {
    ident: String,
}

impl TryFrom<&Ident> for TypeName {
    type Error = Error;

    fn try_from(value: &Ident) -> Result<Self> {
        validate_usr_ident(value).map(|ident| Self { ident })
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
        validate_usr_ident(value).map(|ident| Self { ident })
    }
}

impl FuncName {
    /// Make a function name for primitive methods
    pub fn for_intrinsic(name: &str) -> Self {
        match name {
            // logical
            "not" | "and" | "or" | "xor" 
            // arithmetic
            | "add" | "sub" | "mul" | "div" | "rem"
            // comparison
            | "eq" | "ne" | "lt" | "le" | "ge" | "gt"
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
            _ => panic!("not a primitive method: {}", name),
        }
    }
}

/// Identifier for a reserved macro
pub enum QuantifierMacro {
    Exists,
    Forall,
}

impl TryFrom<&Ident> for QuantifierMacro {
    type Error = Error;

    fn try_from(value: &Ident) -> Result<Self> {
        let name = match value.to_string().as_str() {
            "forall" => Self::Forall,
            "exists" => Self::Exists,
            _ => bail_on!(value, "unknown macro"),
        };
        Ok(name)
    }
}

impl QuantifierMacro {
    /// Extract a macro name from a path
    pub fn from_path(path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        let mut iter = segments.iter().rev();
        let name = match iter.next() {
            None => bail_on!(segments, "invalid path"),
            Some(segment) => {
                let PathSegment { ident, arguments } = segment;
                if !matches!(arguments, PathArguments::None) {
                    bail_on!(arguments, "unexpected arguments");
                }
                ident.try_into()?
            }
        };

        // ensure that there are no more segments
        match iter.next() {
            None => (),
            Some(seg) => bail_on!(seg, "unexpected segment"),
        }
        Ok(name)
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
        validate_usr_ident(value).map(|ident| Self { ident })
    }
}

impl VarName {
    /// Extract a variable name from a pattern
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

    /// Extract a variable name from a path
    pub fn from_path(path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        let mut iter = segments.iter().rev();
        let name = match iter.next() {
            None => bail_on!(segments, "invalid path"),
            Some(segment) => {
                let PathSegment { ident, arguments } = segment;
                if !matches!(arguments, PathArguments::None) {
                    bail_on!(arguments, "unexpected arguments");
                }
                ident.try_into()?
            }
        };

        // ensure that there are no more segments
        match iter.next() {
            None => (),
            Some(seg) => bail_on!(seg, "unexpected segment"),
        }
        Ok(name)
    }

    /// Extract a variable name from an expression path
    pub fn from_expr_path(expr_path: &ExprPath) -> Result<Self> {
        let ExprPath {
            attrs: _,
            qself,
            path,
        } = expr_path;
        bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
        Self::from_path(path)
    }
}

/// An identifier for a ADT variant
#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct ADTBranch {
    adt: TypeName,
    branch: String,
}

impl ADTBranch {
    /// Build manually
    pub fn new(adt: TypeName, branch: String) -> Self {
        Self { adt, branch }
    }

    /// Extract a call target from a path
    pub fn from_path(path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        let mut iter = segments.iter().rev();
        let variant_name = match iter.next() {
            None => bail_on!(segments, "invalid path"),
            Some(segment) => {
                let PathSegment { ident, arguments } = segment;
                if !matches!(arguments, PathArguments::None) {
                    bail_on!(arguments, "unexpected arguments");
                }
                ident.to_string()
            }
        };
        let type_name = match iter.next() {
            None => bail_on!(segments, "invalid path"),
            Some(segment) => {
                let PathSegment { ident, arguments } = segment;
                if !matches!(arguments, PathArguments::None) {
                    bail_on!(arguments, "unexpected arguments");
                }
                ident.try_into()?
            }
        };

        // ensure that there are no more segments
        match iter.next() {
            None => (),
            Some(seg) => bail_on!(seg, "unexpected segment"),
        }
        Ok(Self {
            adt: type_name,
            branch: variant_name,
        })
    }

    /// Getter to type name
    pub fn type_name(&self) -> &TypeName {
        &self.adt
    }

    /// Getter to branch name
    pub fn variant_name(&self) -> &str {
        &self.branch
    }
}

/// Represent a classification of a call target
pub enum CallTargetGuess<'a> {
    Usr(&'a FuncName),
    Sys(&'a str, &'a str),
}

/// Represent a call target
pub struct CallTarget {
    class: String,
    method: FuncName,
}

impl CallTarget {
    /// Extract a call target from a path
    pub fn from_path(path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        let mut iter = segments.iter().rev();
        let method = match iter.next() {
            None => bail_on!(segments, "invalid path"),
            Some(segment) => {
                let PathSegment { ident, arguments } = segment;
                if !matches!(arguments, PathArguments::None) {
                    bail_on!(arguments, "unexpected arguments");
                }
                ident.try_into()?
            }
        };
        let class = match iter.next() {
            None => "Self".to_string(),
            Some(segment) => {
                let PathSegment { ident, arguments } = segment;
                if !matches!(arguments, PathArguments::None) {
                    bail_on!(arguments, "unexpected arguments");
                }
                ident.to_string()
            }
        };

        // ensure that there are no more segments
        match iter.next() {
            None => (),
            Some(seg) => bail_on!(seg, "unexpected segment"),
        }
        Ok(Self { class, method })
    }

    /// Extract a call target from an expression path
    pub fn from_expr_path(expr_path: &ExprPath) -> Result<Self> {
        let ExprPath {
            attrs: _,
            qself,
            path,
        } = expr_path;
        bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
        Self::from_path(path)
    }

    /// Extract a call target from an expression
    pub fn from_expr(expr: &Exp) -> Result<Self> {
        match expr {
            Exp::Path(p) => Self::from_expr_path(p),
            _ => bail_on!(expr, "invalid call target"),
        }
    }

    /// Try to guess whether this target is user-defined or intrinsic
    pub fn as_guess(&self) -> CallTargetGuess {
        if self.class.as_str() == "Self" {
            CallTargetGuess::Usr(&self.method)
        } else {
            CallTargetGuess::Sys(&self.class, &self.method.ident)
        }
    }
}
