use syn::{Ident, Path, PathArguments, PathSegment, Result};

use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::name::ReservedIdent;

/// A convenience wrapper for parsing paths
pub struct PathUtil;

impl PathUtil {
    /// Expect a plain identifier from the path
    pub fn expect_ident(path: &Path) -> Result<&Ident> {
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
        Self::expect_ident(path).and_then(T::from_ident)
    }
}
