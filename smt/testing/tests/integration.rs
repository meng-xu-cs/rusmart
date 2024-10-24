mod model;

use anyhow::anyhow; // for error handling
use datatest_stable::harness; // or writing file-driven or data-driven tests
use rusmart_smt_derive::model;
use std::collections::BTreeSet; // for ordered sets
use std::path::Path; // for file paths
use std::{env, fs}; // for environment variables and file system operations // model is the internal entrypoint for front-end to intermediate representation

static ENV_UPDATE_BASELINE: &str = "UPBL";

/// Checks the consistency between the `mod.rs` file and the directory contents.
///
/// This function reads the `mod.rs` file and collects all module names declared in it.
/// Then it reads the directory containing the `mod.rs` file and collects all submodule names,
/// either as subdirectories containing a `mod.rs` file or as `.rs` files.
/// It then cross-checks to ensure that every module declared in `mod.rs` has a corresponding file/directory,
/// and that every file/directory has a corresponding module declaration in `mod.rs`.
///
/// # Arguments
///
/// * `path` - A reference to the `mod.rs` file to check.
///
/// # Returns
///
/// * `datatest_stable::Result<()>` - Returns `Ok(())` if the check passes, or an error otherwise.
///
/// # Errors
///
/// Returns an error if:
/// - The `mod.rs` file cannot be read.
/// - A line in `mod.rs` is invalid (does not match the `mod xxx;` pattern).
/// - The `mod.rs` does not have a parent directory or the directory containing the `mod.rs` cannot be read.
/// - A sibling file to the `mod.rs` file includes a non-ascii character in its name.
/// - A sibling folder to the `mod.rs` file does not have a child `mod.rs` file.
/// - A module declared in `mod.rs` does not have a corresponding file/directory.
/// - A file/directory does not have a corresponding module declaration in `mod.rs`.
fn check_mod(path: &Path) -> datatest_stable::Result<()> {
    // Create a set to store module names declared in mod.rs
    let mut modules = BTreeSet::new();

    // Read the content of the mod.rs file
    let content = fs::read_to_string(path)?;

    // Iterate over each line in the mod.rs file
    for line in content.lines() {
        let line = line.trim(); // Trim whitespace

        if line.is_empty() {
            continue; // Skip empty lines
        }

        // Check if the line starts with "mod " and ends with ";"
        match line.strip_prefix("mod ").and_then(|e| e.strip_suffix(";")) {
            None => {
                // If not, return an error indicating an invalid line
                return Err(anyhow!("invalid line: {}", line).into());
            }
            Some(base) => {
                // If yes, extract the module name and add it to the set
                modules.insert(base.to_string());
            }
        }
    }

    // Create a set to store file/directory names in the same directory as mod.rs
    let mut files = BTreeSet::new();

    // Get the directory containing the mod.rs file
    let path_dir = path.parent().expect("mod directory");

    // Read the directory entries
    for entry in fs::read_dir(path_dir)? {
        let entry = entry?;
        let name = entry
            .file_name()
            .into_string()
            .expect("ascii filename only");

        // Check if the entry is a directory
        if entry.file_type()?.is_dir() {
            // For directories, check if they contain a mod.rs file
            if !entry.path().join("mod.rs").is_file() {
                return Err(
                    anyhow!("directory {} without mod.rs -- expected a sub-module", name).into(),
                );
            }
            // Add the directory name to the set
            files.insert(name);
        } else {
            // For files, check if they are Rust source files (ending with .rs)
            match name.strip_suffix(".rs") {
                None | Some("mod") => continue, // Skip files that are not .rs files or are mod.rs
                Some(base) => {
                    // Add the base name (without .rs extension) to the set
                    files.insert(base.to_string());
                }
            }
        }
    }

    // Cross-check that every module declared in mod.rs has a corresponding file/directory
    for name in &modules {
        if !files.contains(name) {
            return Err(anyhow!("mod {} without backing file", name).into());
        }
    }

    // Cross-check that every file/directory has a corresponding module declaration in mod.rs
    for name in &files {
        if !modules.contains(name) {
            return Err(anyhow!("file {} without backing mod", name).into());
        }
    }

    // All checks passed
    Ok(())
}

/// Generic test runner for all front-end test cases.
///
/// This function is designed to be used with the `datatest_stable` harness for file-driven tests.
/// It handles test files and compares their output to expected results (if present).
///
/// # Arguments
///
/// * `path` - A reference to the Rust test file to run.
///
/// # Returns
///
/// * `datatest_stable::Result<()>` - Returns `Ok(())` if the test passes, or an error otherwise.
///
/// # Errors
///
/// Returns an error if:
/// - the path does not have a file name or the file name is not ASCII.
/// - the `mod.rs` is not as expected according to the check_mod function.
/// - the test file passes but does not have an `_ok.rs` suffix.
/// - the test file passes but an expected error output file exists while not updating the baseline.
/// - the test file fails but has an `_ok.rs` suffix.
/// - the test file fails but does not have an expected error output file while not updating the baseline.
/// - the test file fails but the expected error output file does not match the actual output while not updating the baseline.
/// - There is an issue with reading or writing test files.
fn test_model(path: &Path) -> datatest_stable::Result<()> {
    // Handle mod.rs files differently by checking module consistency
    let base = path
        .file_name()
        .expect("filename")
        .to_str()
        .expect("ascii-based filename");
    if base == "mod.rs" {
        return check_mod(path); // In practice, this will check if the `parser` directory with a mod.rs exists. If not, an error will be returned. This is because the `mod parser;` line exists inside the top level mod.rs file. The mod model; at the top of the current file is solely for avoiding having unreachable modules.
    }

    // Load existing expected output from a corresponding .exp file
    let path_exp = path.with_extension("exp");
    let expected = if path_exp.exists() {
        Some(fs::read_to_string(&path_exp)?)
    } else {
        None
    };

    // Convention: only files ending with `_ok.rs` are expected to pass successfully
    let ok_hint = base.ends_with("_ok.rs");

    // Check whether we need to update the baseline (.exp files) (e.g., when the output has changed)
    // The ENV_UPDATE_BASELINE is "UPBL". The match env::var_os(...) checks whether the environment variable UPBL has been set to 1. If it has, the update variable is set to true. Otherwise, if it has not been set or has been set to any value other than 1, the update variable is set to false.
    // `UPBL=1 cargo test` => executing this line in the terminal will render `update` as true.
    // `UPBL=2 cargo test` => update = false
    // `cargo test`        => update = false
    let update = match env::var_os(ENV_UPDATE_BASELINE) {
        None => false,
        Some(e) => e.into_string().map_or(false, |s| s == "1"),
    };

    // Run the model function on the test file
    match (model(path), expected) {
        // Test passed, and no expected error output file exists
        (Ok(_), None) => {
            if !ok_hint {
                // If the file is not supposed to pass (does not end with `_ok.rs`), report an error
                return Err(anyhow!("file {:?} with successful test has no `_ok` suffix", path).into());
            }
        }
        // Test passed, but there is an expected error output file (test was expected to fail)
        (Ok(_), Some(exp)) => {
            if !update {
                // If not updating the baseline, report an error with the expected failure message
                return Err(anyhow!("test file {:?} passed while expecting failure\n{}", path, exp).into());
            }
            if !ok_hint {
                // If the file is not supposed to pass, report an error
                return Err(anyhow!("file {:?} with successful test has no `_ok` suffix", path).into());
            }
            // Since updating the baseline, remove the expected error output file
            fs::remove_file(path_exp)?;
        }
        // Test failed, and no expected error output file exists (unexpected failure)
        (Err(err), None) => {
            if !update {
                // If not updating the baseline, report an error with the failure message
                return Err(anyhow!("test file {:?} failed while expecting success\n{}", path, err).into());
            }
            if ok_hint {
                // If the file is supposed to pass, report an error
                return Err(anyhow!("file {:?} with failed test has `_ok` suffix", path).into());
            }
            // Since updating the baseline, write the failure message to the expected output file
            fs::write(path_exp, err.to_string())?;
        }
        // Test failed, and there is an expected error output file (test was expected to fail)
        (Err(err), Some(exp)) => {
            let msg = err.to_string();
            if exp != msg {
                if !update {
                    // If not updating the baseline, report a mismatch between expected and actual outputs
                    return Err(anyhow!(
                        "failure mismatch\n==== expect ===={}\n==== actual ===={}",
                        exp,
                        msg
                    )
                    .into());
                }
                // Since updating the baseline, update the expected output file with the new message
                fs::write(path_exp, msg)?;
            }
            if ok_hint {
                // If the file is supposed to pass, report an error
                return Err(anyhow!("file {:?} with failed test has `_ok` suffix", path).into());
            }
        }
    };

    // All checks passed
    Ok(())
}

// This macro sets up the datatest harness, which runs `test_model` on all `.rs` files in "tests/model" directory.
// This is done recursively, meaning that all subdirectories are also included.
harness!(test_model, "tests/model", r"^.*\.rs$");
