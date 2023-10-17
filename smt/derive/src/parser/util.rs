use syn::{Error, ExprPath, Ident, Pat, PatIdent, Path, PathArguments, PathSegment, Result};

use crate::parser::adt::{ADTPath, TuplePath};
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::expr::CtxtForExpr;
use crate::parser::func::FuncName;
use crate::parser::infer::{TypeRef, TypeUnifier};
use crate::parser::name::{ReservedIdent, UsrFuncName, VarName};
use crate::parser::ty::TypeName;

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
pub enum ExprPathAsTarget {
    /// a local variable
    Var(VarName),
    /// a unit variant in an enum
    EnumUnit(ADTPath),
}

impl ExprPathAsTarget {
    /// Parse from an expression
    pub fn parse<T: CtxtForExpr>(
        ctxt: &T,
        unifier: &mut TypeUnifier,
        expr_path: &ExprPath,
    ) -> Result<Self> {
        let ExprPath {
            attrs: _,
            qself,
            path,
        } = expr_path;
        bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));

        // case by number of path segments
        let parsed = match path.segments.len() {
            1 => Self::Var(PathUtil::expect_ident(path)?.try_into()?),
            2 => Self::EnumUnit(ADTPath::from_path(ctxt, unifier, path)?),
            _ => bail_on!(expr_path, "unrecognized path"),
        };
        Ok(parsed)
    }
}

/// Marks what a path serving as a callee expr can be
pub enum ExprPathAsCallee {
    /// `<type-name>::<func-name>::<ty-args?>(...)`
    FuncWithType(TypeName, FuncName, Option<Vec<TypeRef>>),
    /// `<usr-func-name>::<ty-args?>(...)`
    FuncNoPrefix(UsrFuncName, Option<Vec<TypeRef>>),
    /// `<adt>::<ty-args?>::<variant>(...)`
    CtorEnum(ADTPath),
    /// `<tuple>::<ty-args?>(...)`
    CtorTuple(TuplePath),
}
