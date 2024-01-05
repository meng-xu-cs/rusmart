use std::fs;
use std::path::Path;

use log::{debug, info};
use syn::Result;

use rusmart_utils::config::initialize;

use crate::backend::error::BackendError;
use crate::backend::solvers;
use crate::ir::ctxt::{IRBuilder, IRContext};
use crate::parser::ctxt::Context;

#[cfg(test)]
use proc_macro2::TokenStream;

mod backend;
mod ir;
mod parser;

/// Default pipeline after a context is constructed
fn pipeline(ctxt: Context) -> Result<Vec<IRContext>> {
    let parsed = ctxt
        .parse_generics()?
        .parse_types()?
        .parse_func_sigs()?
        .parse_func_body()?
        .finalize();

    let mut models = vec![];
    for item in parsed.refinements() {
        debug!("processing verification condition for {}", item);
        let ir = IRBuilder::build(&parsed, item);
        models.push(ir);
    }
    Ok(models)
}

/// Internal entrypoint
pub fn derive<P1: AsRef<Path>, P2: AsRef<Path>>(input: P1, output: P2) -> Result<()> {
    // init
    initialize();

    let input = input.as_ref();
    let output = output.as_ref();
    if output.exists() {
        fs::remove_dir_all(output).expect("output directory cleared");
    }
    fs::create_dir_all(output).expect("output directory created");

    // derive
    let models = pipeline(Context::new(input)?)?;
    debug!("derivation completed");

    // solve
    let mut count = 0;
    for ir in models {
        for solver in solvers() {
            count += 1;

            let name = solver.name();
            debug!("[{}] solving {} with {}", count, ir.desc, name);

            let path_wks = output.join(count.to_string());
            match solver.execute(&ir, &path_wks) {
                Ok(()) => (),
                Err(BackendError::NotSupported) => {
                    info!(
                        "[{}] solving {} with {}: not supported",
                        count, ir.desc, name
                    );
                }
            }
        }
    }

    Ok(())
}

#[cfg(test)]
/// A shortcut to run tests
pub fn test_on_stream(stream: TokenStream) -> Result<()> {
    Context::new_from_stream(stream)
        .and_then(pipeline)
        .map(|_| ())
}
