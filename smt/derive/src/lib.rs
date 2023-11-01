use std::path::Path;

use log::debug;
use rusmart_utils::config::initialize;
use syn::Result;

use crate::parser::ctxt::Context;

#[cfg(test)]
use proc_macro2::TokenStream;

mod ir;
mod parser;

/// Default pipeline after a context is constructed
fn pipeline(ctxt: Context) -> Result<()> {
    let parsed = ctxt
        .parse_generics()?
        .parse_types()?
        .parse_func_sigs()?
        .parse_func_body()?;
    for vc in parsed.refinements() {
        debug!("processing verification condition {}", vc);
    }
    Ok(())
}

/// Internal entrypoint
pub fn derive<P: AsRef<Path>>(input: P) -> Result<()> {
    initialize();

    let path_crate = input.as_ref();
    debug!("deriving for crate {}", path_crate.to_string_lossy());
    pipeline(Context::new(path_crate)?)?;
    debug!("derivation completed");

    Ok(())
}

#[cfg(test)]
/// A shortcut to run tests
pub fn test_on_stream(stream: TokenStream) -> Result<()> {
    Context::new_from_stream(stream).and_then(pipeline)
}
