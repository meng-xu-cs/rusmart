use std::path::Path;

use datatest_stable::{harness, Result};

fn run(path: &Path) -> Result<()> {
    println!("{}", path.to_string_lossy());
    Ok(())
}

harness!(run, "tests/parser", r"^.*\.rs");
