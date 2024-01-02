use std::process::Command;

use anyhow::{bail, Result};

use rusmart_utils::config::NUM_CPU_CORES;

use crate::dep::{Artifact, Dependency};

// path constants
static PATH_REPO: [&str; 2] = ["deps", "z3"];

/// Represent the dependency: Z3
pub struct DepZ3 {}

impl Dependency for DepZ3 {
    fn repo_path_from_root() -> &'static [&'static str] {
        &PATH_REPO
    }

    fn list_configurations(artifact: &Artifact) -> Result<()> {
        let mut cmd = Command::new("python3");
        cmd.arg("scripts/mk_make.py")
            .arg("--help")
            .current_dir(&artifact.src);

        let status = cmd.status()?;
        if !status.success() {
            bail!("list configuration failed");
        }
        Ok(())
    }

    fn build(artifact: &Artifact) -> Result<()> {
        // config
        let mut cmd = Command::new("python3");
        cmd.arg("scripts/mk_make.py")
            .arg(format!(
                "--prefix={}",
                artifact.dst.to_str().expect("ascii path")
            ))
            .arg("--debug")
            .arg("--single-threaded")
            .current_dir(&artifact.src);
        let status = cmd.status()?;
        if !status.success() {
            bail!("configure failed");
        }
        let path_build = artifact.src.join("build");

        // build
        let mut cmd = Command::new("make");
        cmd.arg(format!("-j{}", *NUM_CPU_CORES))
            .current_dir(&path_build);
        let status = cmd.status()?;
        if !status.success() {
            bail!("build failed");
        }

        // install
        let mut cmd = Command::new("make");
        cmd.arg("install").current_dir(&path_build);
        let status = cmd.status()?;
        if !status.success() {
            bail!("install failed");
        }

        // done
        Ok(())
    }
}
