use std::process::Command;

use anyhow::{bail, Result};

use crate::dep::{Artifact, Dependency};

// path constants
static PATH_REPO: [&str; 2] = ["deps", "cvc5"];

/// Represent the dependency: CVC5
pub struct DepCVC5 {}

impl Dependency for DepCVC5 {
    fn repo_path_from_root() -> &'static [&'static str] {
        &PATH_REPO
    }

    fn list_configurations(artifact: &Artifact) -> Result<()> {
        let mut cmd = Command::new("./configure.sh");
        cmd.arg("--help").current_dir(&artifact.src);

        let status = cmd.status()?;
        if !status.success() {
            bail!("list configuration failed");
        }
        Ok(())
    }

    fn build(artifact: &Artifact) -> Result<()> {
        // third-party
        let mut cmd = Command::new(artifact.src.join("contrib").join("get-glpk-cut-log"));
        cmd.current_dir(&artifact.src);
        let status = cmd.status()?;
        if !status.success() {
            bail!("contrib/get-glpk-cut-log failed");
        }

        // config
        let mut cmd = Command::new("./configure.sh");
        cmd.arg("debug")
            .arg(format!(
                "--prefix={}",
                artifact.dst.to_str().expect("ascii path")
            ))
            .arg("--ninja")
            .arg("--gpl")
            .arg("--auto-download");

        // NOTE: CLN-EP cannot be built on MacOS
        #[cfg(not(target_os = "macos"))]
        cmd.arg("--best");

        cmd.current_dir(&artifact.src);
        let status = cmd.status()?;
        if !status.success() {
            bail!("configure failed");
        }
        let path_build = artifact.src.join("build");

        // build
        let mut cmd = Command::new("ninja");
        cmd.current_dir(&path_build);
        let status = cmd.status()?;
        if !status.success() {
            bail!("build failed");
        }

        // install
        let mut cmd = Command::new("ninja");
        cmd.arg("install").current_dir(&path_build);
        let status = cmd.status()?;
        if !status.success() {
            bail!("install failed");
        }

        // done
        Ok(())
    }
}
