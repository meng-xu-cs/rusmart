use std::path::Path;

use anyhow::Result;

use rusmart_smt_derive::parser::ctxt::Context;
use rusmart_smt_testing::test_suite;

fn run(path: &Path) -> Result<()> {
    Context::new(path)?;
    Ok(())
}

test_suite!(parser, "tests/parser", crate::run);
