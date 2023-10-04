use std::path::Path;

mod parse_ctxt;
mod parse_expr;
mod parse_func;
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
    parse_ctxt::Context::new(path_crate)?.analyze()?;
    Ok(())
}
