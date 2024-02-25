mod model;

use std::collections::BTreeSet;
use std::path::Path;
use std::{env, fs};

use anyhow::anyhow;
use datatest_stable::harness;

use rusmart_smt_derive::model;

static ENV_UPDATE_BASELINE: &str = "UPBL";

fn check_mod(path: &Path) -> datatest_stable::Result<()> {
    // read the file
    let mut modules = BTreeSet::new();
    let content = fs::read_to_string(path)?;
    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        match line.strip_prefix("mod ").and_then(|e| e.strip_suffix(";")) {
            None => {
                return Err(anyhow!("invalid line: {}", line).into());
            }
            Some(base) => {
                modules.insert(base.to_string());
            }
        }
    }

    // read the directory
    let mut files = BTreeSet::new();
    let path_dir = path.parent().expect("mod directory");
    for entry in fs::read_dir(path_dir)? {
        let entry = entry?;
        let name = entry
            .file_name()
            .into_string()
            .expect("ascii filename only");
        if entry.file_type()?.is_dir() {
            if !entry.path().join("mod.rs").is_file() {
                return Err(anyhow!("no sub-module {}", name).into());
            }
            files.insert(name);
        } else {
            match name.strip_suffix(".rs") {
                None | Some("mod") => continue,
                Some(base) => {
                    files.insert(base.to_string());
                }
            }
        }
    }

    // cross-check
    for name in &modules {
        if !files.contains(name) {
            return Err(anyhow!("mod {} without backing file", name).into());
        }
    }
    for name in &files {
        if !modules.contains(name) {
            return Err(anyhow!("file {} without backing mod", name).into());
        }
    }

    Ok(())
}

/// Generic test runner for all front-end test cases
fn test_model(path: &Path) -> datatest_stable::Result<()> {
    // mod.rs are tested differently
    let base = path
        .file_name()
        .expect("filename")
        .to_str()
        .expect("ascii-based filename");
    if base == "mod.rs" {
        return check_mod(path);
    }

    // load existing message
    let path_exp = path.with_extension("exp");
    let expected = if path_exp.exists() {
        Some(fs::read_to_string(&path_exp)?)
    } else {
        None
    };

    // convention: only file ending with `_ok` can complete with success
    let ok_hint = base.ends_with("_ok.rs");

    // check whether we need to update the baseline
    let update = match env::var_os(ENV_UPDATE_BASELINE) {
        None => false,
        Some(e) => e.into_string().map_or(false, |e| e == "1"),
    };

    // run the test
    match (model(path), expected) {
        (Ok(_), None) => {
            if !ok_hint {
                return Err(anyhow!("test passed without the `_ok` suffix").into());
            }
        }
        (Ok(_), Some(exp)) => {
            if !update {
                return Err(anyhow!("test passed while expecting failure\n{}", exp).into());
            }
            if !ok_hint {
                return Err(anyhow!("test passed without the `_ok` suffix").into());
            }
            fs::remove_file(path_exp)?;
        }
        (Err(err), None) => {
            if !update {
                return Err(anyhow!("test failed while expecting success\n{}", err).into());
            }
            if ok_hint {
                return Err(anyhow!("test failed with the `_ok` suffix").into());
            }
            fs::write(path_exp, err.to_string())?;
        }
        (Err(err), Some(exp)) => {
            let msg = err.to_string();
            if exp != msg {
                if !update {
                    return Err(anyhow!(
                        "failure mismatch\n==== expect ===={}\n==== actual ===={}",
                        exp,
                        msg
                    )
                    .into());
                }
                fs::write(path_exp, msg)?;
            }
            if ok_hint {
                return Err(anyhow!("test failed with the `_ok` suffix").into());
            }
        }
    };

    // done
    Ok(())
}

harness!(test_model, "tests/model", r"^.*\.rs$");
