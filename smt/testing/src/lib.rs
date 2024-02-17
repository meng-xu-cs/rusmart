use std::path::Path;
use std::{env, fs};

use anyhow::{bail, Result};

pub fn test<F>(path: &Path, runner: F) -> Result<()>
where
    F: FnOnce(&Path) -> Result<()>,
{
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
    match (runner(path), expected) {
        (Ok(()), None) => (),
        (Ok(()), Some(exp)) => {
            if !update {
                bail!("test passed while expecting failure\n{}", exp);
            }
            fs::remove_file(path_exp)?;
        }
        (Err(err), None) => {
            if !update {
                bail!("test failed while expecting success\n{}", err)
            }
            fs::write(path_exp, err.to_string())?;
        }
        (Err(err), Some(exp)) => {
            let msg = err.to_string();
            if exp != msg {
                if !update {
                    bail!(
                        "failure mismatch\n==== expect ===={}\n==== actual ===={}",
                        exp,
                        msg
                    );
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
    ($name:ident, $path:literal, $runner:path) => {
        mod $name {
            automod::dir!($path);
            pub fn tester(path: &std::path::Path) -> datatest_stable::Result<()> {
                $crate::test(path, $runner).map_err(|e| e.into())
            }
        }
        datatest_stable::harness!($name::tester, $path, r"^.*\.rs$");
    };
}
