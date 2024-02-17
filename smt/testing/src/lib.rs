use std::path::Path;
use std::{env, fs};

use anyhow::anyhow;

use rusmart_smt_derive::model;

pub fn test_model(path: &Path) -> datatest_stable::Result<()> {
    // load existing message
    let path_exp = path.with_extension(".exp");
    let expected = if path_exp.exists() {
        Some(fs::read_to_string(&path_exp)?)
    } else {
        None
    };

    // check whether we need to update the baseline
    let update = match env::var_os("UPBL") {
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

#[macro_export]
macro_rules! test_suite {
    ($name:ident, $path:literal) => {
        mod $name {
            automod::dir!($path);
        }
        datatest_stable::harness!($crate::test_model, $path, r"^.*\.rs$");
    };
}
