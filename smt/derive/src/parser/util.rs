use syn::{Error, ExprPath, Ident, Pat, PatIdent, Path, PathArguments, PathSegment, Result};

use crate::parser::adt::ADTBranch;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::name::{ReservedIdent, VarName};

/// A convenience wrapper for parsing paths
pub struct PathUtil;

impl PathUtil {
    /// Expect a plain identifier from the path
    fn expect_ident(path: &Path) -> Result<&Ident> {
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

    /// Expect a reserved identifier from the path
    pub fn expect_ident_reserved<T: ReservedIdent>(path: &Path) -> Result<T> {
        let ident = Self::expect_ident(path)?;
        match T::from_str(ident.to_string().as_str()) {
            None => bail_on!(ident, "not an intrinsic trait"),
            Some(v) => Ok(v),
        }
    }
}

/// A convenience wrapper for parsing patterns
pub struct PatUtil;

impl PatUtil {
    /// Expect a plain identifier from the pattern
    fn expect_ident(pat: &Pat) -> Result<&Ident> {
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

    /// Expect a name that can be converted from an ident
    pub fn expect_name<'a, T: TryFrom<&'a Ident, Error = Error>>(pat: &'a Pat) -> Result<T> {
        Self::expect_ident(pat).and_then(T::try_from)
    }
}

/// Marks what a path serving as a standalone expr can be
pub enum ExprPathStandalone {
    /// a local variable
    Var(VarName),
    /// a unit variant in an enum
    EnumUnit(ADTBranch),
}

impl ExprPathStandalone {
    /// Parse from an expression
    pub fn parse(expr_path: &ExprPath) -> Result<Self> {
        // extract segments
        let ExprPath {
            attrs: _,
            qself,
            path,
        } = expr_path;
        bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));

        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        // collect segments
        let mut idents = vec![];
        for segment in segments {
            let PathSegment { ident, arguments } = segment;
            if !matches!(arguments, PathArguments::None) {
                bail_on!(arguments, "unexpected arguments");
            }
            idents.push(ident);
        }

        // decide on what to do with the idents
        let parsed = match idents.len() {
            1 => {
                // handle variable reference
                let name = idents.into_iter().next().unwrap().try_into()?;
                Self::Var(name)
            }
            2 => {
                // handle adt construction
                let mut iter = idents.into_iter();
                let adt = iter.next().unwrap().try_into()?;
                let variant = iter.next().unwrap().to_string();
                Self::EnumUnit(ADTBranch::new(adt, variant))
            }
            _ => bail_on!(expr_path, "unrecognized path"),
        };
        Ok(parsed)
    }
}
