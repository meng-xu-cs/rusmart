use std::path::Path;

use log::debug;
use rusmart_utils::config::initialize;
use syn::Result;

use crate::parser::ctxt::Context;

mod parser;

/// Internal entrypoint for ergonomic error propagation
pub fn derive<P: AsRef<Path>>(input: P) -> Result<()> {
    initialize();

    let path_crate = input.as_ref();
    debug!("deriving for crate {}", path_crate.to_string_lossy());
    Context::new(path_crate)?
        .parse_generics()?
        .parse_types()?
        .parse_func_sigs()?;
    debug!("derivation completed");

    Ok(())
}
