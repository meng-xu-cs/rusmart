use std::path::Path;

use rusmart_smt_derive::derive;

// mark them as dependencies
pub mod derive;

fn run_test(path: &Path) -> datatest_stable::Result<()> {
    derive(path).map_err(|e| e.into())
}

datatest_stable::harness!(run_test, "tests/derive", r"^*.*\.rs$");
