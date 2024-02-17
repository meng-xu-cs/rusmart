mod model;

use std::path::Path;
use std::{env, fs};

use anyhow::anyhow;
use datatest_stable::harness;

use rusmart_smt_derive::model;

static ENV_UPDATE_BASELINE: &str = "UPBL";

/// Generic test runner for all front-end test cases
fn test_model(path: &Path) -> datatest_stable::Result<()> {
    // load existing message
    let path_exp = path.with_extension(".exp");
    let expected = if path_exp.exists() {
        Some(fs::read_to_string(&path_exp)?)
    } else {
        None
    };

    // check whether we need to update the baseline
    let update = match env::var_os(ENV_UPDATE_BASELINE) {
        None => false,
        Some(e) => e.into_string().map_or(false, |e| e == "1"),
    };

    // run the test
    match (model(path), expected) {
        (Ok(_), None) => (),
        (Ok(_), Some(exp)) => {
            if !update {
                return Err(anyhow!("test passed while expecting failure\n{}", exp).into());
            }
            fs::remove_file(path_exp)?;
        }
        (Err(err), None) => {
            if !update {
                return Err(anyhow!("test failed while expecting success\n{}", err).into());
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
        }
    };

    // done
    Ok(())
}

harness!(test_model, "tests/model", r"^.*\.rs$");
