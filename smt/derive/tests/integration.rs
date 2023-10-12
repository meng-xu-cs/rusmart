mod derive;

use std::path::Path;

fn run_test(path: &Path) -> datatest_stable::Result<()> {
    println!("{}", path.to_string_lossy());
    Ok(())
}

datatest_stable::harness!(run_test, "tests/derive", r"^*.*\.rs$");
