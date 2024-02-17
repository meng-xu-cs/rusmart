use std::path::Path;

use anyhow::Result;

use rusmart_smt_testing::test_suite;

fn run(path: &Path) -> Result<()> {
    println!("{}", path.to_string_lossy());
    Ok(())
}

test_suite!(parser, "tests/parser", crate::run);
