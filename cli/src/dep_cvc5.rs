//! CVC5 dependency
//!
//! This module contains the DepCVC5 struct, which represents the CVC5 dependency.
//! The DepCVC5 struct implements the Dependency trait, which provides the functionality to configure and build the CVC5 dependency.
//!

// ------------------------------------------DEPENDENCIES---------------------------------------//

// the `std::process::Command` module provides the Command struct to spawn child processes
use std::process::Command;
// the `anyhow` crate provides the `bail` macro to return an error
// The Result type is a type alias for `Result<T, anyhow::Error>`.
use anyhow::{bail, Context, Result};
// artifact struct contains the base, src and dst paths
use crate::dep::{Artifact, Dependency};

// ------------------------------------------DEFINITIONS------------------------------------------//

// path constants
static PATH_REPO: [&'static str; 2] = ["deps", "cvc5"];

/// Represent the dependency: CVC5
#[derive(Debug, PartialEq, Eq)]
pub struct DepCVC5 {}

// ------------------------------------------IMPLEMENTATION------------------------------------//

impl Dependency for DepCVC5 {
    /// Get the path to the CVC5 repository from the root of the workspace (deps/cvc5)
    fn repo_path_from_root() -> &'static [&'static str] {
        &PATH_REPO
    }

    /// print the configurations of the CVC5 dependency
    /// This function runs the `./configure.sh --help` command in the CVC5 source directory.
    /// Please consult https://github.com/cvc5/cvc5/blob/main/configure.sh for more information on the configuration options.
    /// Note that the source directory of the artifact should contain a clone of the CVC5 repository. Otherwise, this function will fail and the error will be spawned by the ? operator.
    fn list_configurations(artifact: &Artifact) -> Result<()> {
        let mut cmd = Command::new("./configure.sh");
        cmd.arg("--help").current_dir(&artifact.src);

        let output = cmd.output().context(format!(
            "Maybe the CVC5 repository is not cloned in the source directory: {}",
            artifact.src.display()
        ))?;
        if !output.status.success() {
            bail!("list configuration failed");
        }
        Ok(())
    }

    /// Build the CVC5 dependency
    /// This function runs the following command in the CVC5 source directory:
    /// - `./configure.sh debug --prefix=<dst> --ninja --gpl --auto-download`
    /// A `build` directory is created in the source directory during the config and the following commands are run in the `build` directory:
    /// - `ninja`
    /// - `ninja install`
    fn build(artifact: &Artifact) -> Result<()> {
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
        cmd.current_dir(&artifact.src);

        let output = cmd.output()?;
        if !output.status.success() {
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

// ------------------------------------------TESTS-----------------------------------------------//

#[cfg(test)]
mod tests {
    use super::*;
    use crate::git::GitRepo;
    use std::fs;
    use std::io::Write;
    use std::path::PathBuf;
    use rusmart_utils::config::WKS;

    #[test]
    /// testing the repo_path_from_root function for the DepCVC5 type.
    fn test_repo_path_from_root_cvc5() {
        assert_eq!(DepCVC5::repo_path_from_root(), PATH_REPO);
    }

    /// This helper function runs the `git submodule update --init --recursive` command in the workspace root.
    /// This is necessary to clone the cvc5 repository to the deps/cvc5 directory.
    fn make_cvc5() {
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

    /// This helper function creates an artifact for the cvc5 dependency.
    fn make_artifact() -> (PathBuf, Artifact) {
        // cloning the cvc5 repository in the deps/cvc5 directory
        make_cvc5();

        // getting the path to the deps/cvc5
        let mut path = WKS.base.clone();
        path = path.join(DepCVC5::repo_path_from_root().join("/"));

        // making a git repo struct from the cvc5
        let mut git_repo = GitRepo::new(path, None).expect("could not create git repo");

        // creating an artifact. bascially this creates a tmp folder in the current directory
        // inside of which two directories dst and src are created.
        // dst is empty but in src, the cvc5 is cloned and checked out.
        let mut base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        base = base.join("tmp");
        let artifact = Artifact::init(&mut git_repo, base.clone()).expect("could not initialize artifact");

        (base, artifact)
    }

    #[test]
    /// Testing the list_configurations function for the DepCVC5 type.
    /// This should be successful.
    fn test_list_configurations_cvc5() {
        let (base, artifact) = make_artifact();
        // this should be okay
        let res = DepCVC5::list_configurations(&artifact);
        assert!(res.is_ok());

        // clean up
        fs::remove_dir_all(&base).expect("could not remove tmp directory");
    }

    /// Helper to create a failure mock configure.sh file inside the src directory of the artifact
    fn create_mock_configure_failure(artifact: &Artifact) {
        let configure_path = artifact.src.join("configure.sh");

        // Create or overwrite the configure.sh file
        // this will fail because the configure.sh file will exit with status 1
        let mut file = fs::File::create(configure_path).expect("Failed to create configure.sh");
        writeln!(file, "#!/bin/sh\necho 'Configuring...'\nexit 1")
            .expect("Failed to write to configure.sh");

        // Make it executable
        // This is only for Unix like systems
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(artifact.src.join("configure.sh"))
                .expect("Failed to get metadata")
                .permissions();
            perms.set_mode(0o755); // this is the same as chmod 755 and makes the file executable
            fs::set_permissions(artifact.src.join("configure.sh"), perms)
                .expect("Failed to set permissions");
        }
    }

    #[test]
    // This is the failure case for list_configurations
    fn test_list_configurations_failure() {
        let (base, artifact) = make_artifact();
        // Create a mock configure.sh file in the src directory that will fail
        create_mock_configure_failure(&artifact);

        // Call the function and assert it fails
        let result = DepCVC5::list_configurations(&artifact);
        assert!(result.is_err(), "list_configurations should fail");
        assert_eq!(
            result.err().expect("could not get error message").to_string(),
            "list configuration failed"
        );

        // clean up
        fs::remove_dir_all(&base).expect("could not remove tmp directory");
    }

    // theoretically, the build function can be tested with the below test function.
    // however, the build function takes a long time to run and thus, it is not tested.
    // #[test]
    // /// testing the build function for the DepCVC5 type.
    // fn test_build_cvc5() {
    //     let (base, artifact) = make_artifact();
    //     // this should be okay
    //     let res = DepCVC5::build(&artifact);
    //     assert!(res.is_ok());

    //     // clean up
    //     fs::remove_dir_all(&base).expect("could not remove tmp directory");
    // }
}
