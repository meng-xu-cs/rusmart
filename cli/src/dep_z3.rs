use std::fs;
use std::process::Command;

use anyhow::{bail, Result};

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
        let mut cmd = Command::new("cmake");
        cmd.arg("-LAH")
            .arg(&artifact.src)
            .current_dir(&artifact.dst);
        let status = cmd.status()?;
        if !status.success() {
            bail!("list configuration failed");
        }
        Ok(())
    }

    fn build(artifact: &Artifact) -> Result<()> {
        // config
        let path_build = artifact.src.join("build");
        fs::create_dir(&path_build)?;

        let mut cmd = Command::new("cmake");
        cmd.arg("-G")
            .arg("Ninja")
            .arg("-DCMAKE_BUILD_TYPE=Debug")
            .arg("-DZ3_SINGLE_THREADED=TRUE")
            .arg(&artifact.src)
            .current_dir(&path_build);
        let status = cmd.status()?;
        if !status.success() {
            bail!("configure failed");
        }

        // build
        let mut cmd = Command::new("cmake");
        cmd.arg("--build").arg(&path_build);
        let status = cmd.status()?;
        if !status.success() {
            bail!("build failed");
        }

        // install
        let mut cmd = Command::new("cmake");
        cmd.arg("--install")
            .arg(&path_build)
            .arg("--prefix")
            .arg(&artifact.dst);
        let status = cmd.status()?;
        if !status.success() {
            bail!("install failed");
        }

        // done
        Ok(())
    }
}
