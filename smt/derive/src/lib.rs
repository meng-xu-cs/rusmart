use std::path::Path;

mod err;

mod parse_ctxt;
mod parse_expr;
mod parse_expr_intrinsic;
mod parse_expr_match;
mod parse_func;
mod parse_path;
mod parse_type;

/// Entrypoint to the SMT model derivation process
pub fn derive<P: AsRef<Path>>(input: P) {
    let path_crate = input.as_ref();
    log::debug!("deriving for crate {}", path_crate.to_string_lossy());
    derive_internal(path_crate).unwrap_or_else(|err| panic!("{}", err));
    log::debug!("derivation completed");
}

/// Internal entrypoint for ergonomic error propagation
fn derive_internal(path_crate: &Path) -> syn::Result<()> {
    parse_ctxt::Context::new(path_crate)?
        .analyze_type()?
        .analyze_func_sig()?
        .analyze_func_body()?;
    Ok(())
}
