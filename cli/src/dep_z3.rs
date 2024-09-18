//! Z3 dependency
//!
//! This module contains the DepZ3 struct, which represents the Z3 dependency.
//! The DepZ3 struct implements the Dependency trait, which provides the functionality to configure and build the Z3 dependency.

// ------------------------------------------DEPENDENCIES---------------------------------------//

// the `std::process::Command` module provides the Command struct to spawn child processes
use std::process::Command;
// the `anyhow` crate provides the `bail` macro to return an error
// The Result type is a type alias for `Result<T, anyhow::Error>`.
use anyhow::{bail, Context, Result};
// artifact struct contains the base, src and dst paths
use crate::dep::{Artifact, Dependency};
// interacting with the file system
use std::fs;

// ------------------------------------------DEFINITIONS------------------------------------------//

// path constants
static PATH_REPO: [&'static str; 2] = ["deps", "z3"];

/// Represent the dependency: Z3
#[derive(Debug, PartialEq, Eq)]
pub struct DepZ3 {}

// ------------------------------------------IMPLEMENTATIONS------------------------------------//

impl Dependency for DepZ3 {
    /// Get the path to the Z3 repository from the root of the workspace (deps/z3)
    fn repo_path_from_root() -> &'static [&'static str] {
        &PATH_REPO
    }

    /// print the configurations of the Z3 dependency
    /// This function runs the `cmake -LAH <src>` command in the Z3 destination directory.
    /// Please consult https://github.com/Z3Prover/z3 for more information on the configuration options.
    /// Note that the source directory of the artifact should contain a clone of the Z3 repository. Otherwise, this function will fail and the error will be spawned by the ? operator.
    fn list_configurations(artifact: &Artifact) -> Result<()> {
        let mut cmd = Command::new("cmake");
        cmd.arg("-LAH")
            .arg(&artifact.src)
            .current_dir(&artifact.dst);
        let output = cmd.output().context(format!(
            "Maybe the Z3 repository is not cloned in the source directory: {}",
            artifact.src.display()
        ))?;
        if !output.status.success() {
            bail!("list configuration failed");
        }
        Ok(())
    }

    /// Build the Z3 dependency
    /// This function first creates the build directory in the source directory and runs the following commands in the build directory:
    /// - `cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -DZ3_SINGLE_THREADED=TRUE <src>`
    /// - `cmake --build .`
    /// - `cmake --install . --prefix <dst>`
    /// Note that the source directory of the artifact should contain a clone of the Z3 repository. Otherwise, this function will fail and the error will be spawned by the ? operator.
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

// ------------------------------------------TESTS------------------------------------//

#[cfg(test)]
mod tests {
    use super::*;
    use crate::git::GitRepo;
    use std::path::PathBuf;
    use rusmart_utils::config::WKS;

    #[test]
    // testing the repo_path_from_root function for the DepZ3 type.
    fn test_repo_path_from_root_z3() {
        assert_eq!(DepZ3::repo_path_from_root(), PATH_REPO);
    }

    /// This helper function runs the `git submodule update --init --recursive` command in the workspace root.
    /// This is necessary to clone the z3 repository to the deps/z3 directory.
    fn make_z3() {
        let path = WKS.base.clone();

        let mut command = Command::new("git");
        command
            .args(["submodule", "update", "--init", "--recursive"])
            .current_dir(path.clone());

        let output = command.output().expect("could not update submodules...");
        if !output.status.success() {
            panic!("could not update submodules...");
        }
    }

    // create an artifact for the z3 dependency
    fn make_artifact() -> (PathBuf, Artifact) {
        // cloning the z3 repository in the deps/z3 directory
        make_z3();

        // getting the path to the deps/z3
        let mut path = WKS.base.clone();
        path = path.join(DepZ3::repo_path_from_root().join("/"));

        // making a git repo struct from the z3
        let mut git_repo = GitRepo::new(path, None).expect("could not create git repo");

        // creating an artifact. bascially this creates a tmp folder in the current directory
        // inside of which two directories dst and src are created.
        // dst is empty but in src, the z3 is cloned and checked out.
        let mut base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        base = base.join("tmp");
        let artifact = Artifact::init(&mut git_repo, base.clone()).expect("could not initialize artifact");

        (base, artifact)
    }

    #[test]
    /// Testing the list_configurations function for the DepZ3 type.
    /// This should be successful.
    fn test_list_configurations_z3() {
        let (path, artifact) = make_artifact();

        let config = DepZ3::list_configurations(&artifact);
        assert!(config.is_ok());

        // clean up
        fs::remove_dir_all(&path).expect("could not remove tmp directory");
    }

    // theoretically, the build function can be tested with the below test function.
    // however, the build function takes a long time to run and thus, it is not tested.
    // #[test]
    // // testing the build function for the DepZ3 type.
    // fn test_build_z3() {
    //     let (base, artifact) = make_artifact();
    //     // this should be okay
    //     let res = DepZ3::build(&artifact);
    //     assert!(res.is_ok());

    //     // clean up
    //     fs::remove_dir_all(&base).expect("could not remove tmp directory");
    // }
}
