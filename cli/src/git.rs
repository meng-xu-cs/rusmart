//! This module provides a simple abstraction for interacting with Git repositories.
//!
//! The `GitRepo` struct encapsulates the path to a Git repository and the commit hash.
//! It provides the following functionality:
//!
//! - Create a new instance of `GitRepo`.
//! - Retrieve the current commit hash of the repository.
//! - Clone & Checkout the repository into a new directory.
//!

// ------------------------------------------------------------------------------------------------//

// PathBuf & Path both represent a path on the filesystem.
// PathBuf is owned, mutable, heap-allocated and growable.
// Path is borrowed, immutable, stack-allocated and fixed-size.
// The relation between PathBuf and Path is like String and str.
// Note that a Path is not internally represented as an UTF-8 string, but instead is stored as an OsString
use std::path::{Path, PathBuf};
// Command is a struct representing a command to be run in a child process.
// It is mainly used to spawn new processes for example running a shell command from the Rust program.
use std::process::Command;
// The Result type is a type that represents either success (Ok) or failure (Err).
// It is a type alias of the std::result::Result type: pub type Result<T, E = Error> = std::result::Result<T, E>;
// anyhow function is used to create an error from a string or an error type that can be converted into an anyhow::Error.
// bail function is used to return an error from a function without explicitly mentioning return. bail!(....) = return Err(anyhow!(...))
// The Context trait allows you to add extra context when an operation returns an error. This is done by calling the context or with_context methods on a Result.
use anyhow::{anyhow, bail, Context, Result};

//---------------------------------------------------------------------------------------------------//

// These traits are derived for writing unit tests.
#[derive(Debug, PartialEq, Eq, Clone)]
/// Represents a Git-based repository
/// This struct encapsulates the path to a Git repository and the commit hash.
pub struct GitRepo {
    path: PathBuf,
    commit: String,
}

impl GitRepo {
    /// Creates a new instance of `GitRepo` (a representation of the repository).
    ///
    /// # Arguments
    ///
    /// * `path` - The path to the Git repository.
    /// * `version` - An optional reference to a specific commit hash or branch name.
    ///
    /// # Returns
    ///
    /// * `Result<Self>` - Returns an instance of `GitRepo` on success, or an error if the commit probing fails.
    ///
    /// The function runs `git rev-list -n 1 <version>`.
    /// version has two options: None or Some(&str).
    /// 1) If the version is None, `git rev-list -n 1 Head` will be executed.
    /// 2) Otherwise, `git rev-list -n 1 <version:&str>` will be executed.
    ///
    /// The `git rev-list` provides a list of all the previous hash commits.
    /// The -n 1 parameters mean give the first one in the list (which is the latest hash commit given that the commits are ordered in a reverse chronological order).
    /// The version in `git rev-list -n 1 <version>` means that give the latest commit hash from the specified version.
    /// The path is specified to mention from which directory this command should be executed. In other words, the command is run in the directory specified by the `path` variable.
    ///
    ///
    /// # Errors:
    /// The cmd.output() gives a Result<Output>. The ? in output = cmd.output()?; propagates the error.
    /// The phrase !output.status.success() may seem redundant but the ? propagates the system level error. Meaning that if the path doesn't exist, the user doesn't have permission to run the command, they will all be propagated here.
    /// output.status.success() checks the application level errors. This occurs when the command has run itself but an error occurs with a non existent status code. For example, if the path exists but is not a git repository, or the network connection has failed, or the flags indicated for the git command are invalid, etc.
    ///
    /// The output struct has three fields:
    /// stdout, stderr, and status. When the status code is zero it indicates success and the stderr will be empty in this case. The output() runs the command synchronously meaning the program will wait until the command is finished. For a Asynchronously command run we can use spawn().
    pub fn new(path: PathBuf, version: Option<&str>) -> Result<Self> {
        let mut cmd = Command::new("git"); // we cannot chain the arg to this because then the Command struct will be temporarily created.

        cmd.arg("rev-list")
            .arg("-n")
            .arg("1")
            .arg(version.unwrap_or("HEAD"))
            .current_dir(&path);

        // Execute the command and capture the output
        let output = cmd.output().context(format!(
            "The issue might be because the path {:?} does not exist.",
            path
        ))?;
        if !output.status.success() {
            bail!("commit probing failed");
        }

        // Convert the output to a String and trim any whitespace
        // We have not used String::from_utf8_lossy because this method will replace invalid UTF-8 sequences with the "replacement character" (�), ensuring that the conversion always succeeds.
        let commit = String::from_utf8(output.stdout)
            .context("Failed to parse std output as UTF-8")?
            .trim()
            .to_string();

        // we return the GitRepo that is the path directory with the latest commit hash.
        // So basically this function only attaches the commit hash to the specified path.
        Ok(Self { path, commit })
    }

    /// Retrieves the current commit hash of the repository.
    ///
    /// # Returns
    ///
    /// * `&str` - A reference to the commit hash.
    pub fn commit(&self) -> &str {
        &self.commit
    }

    /// Checks out the repository into a new directory.
    ///
    /// # Arguments
    ///
    /// * `path_src` - The source path where the repository will be checked out.
    ///
    /// # Returns
    ///
    /// * `Result<()>` - Returns an empty result on success or an error if the checkout fails.
    ///
    /// This program runs the `git clone <path1> <path2>` and then inside the  <path2> directory it runs the `git checkout <commit>` command. path1 and commit are the path and commit hash of the GitRepo instance.
    /// So basically it clones the git repo into the destination path. Then checks out the repo to the specified commit.
    ///
    /// # Errors
    /// If the path already exists the checkout is not possible. This is because if path_src already exists and has some files in it, or is a git repository itself for example, then git will throw an error and the function will panic. Only if the path is empty it is okay and git will clone the repository into the destination path. To simplify the function we will accept a non existent path as the second argument of the checkout function.
    pub fn checkout(&mut self, path_src: &Path) -> Result<()> {
        if path_src.exists() {
            bail!(
                "checkout path already exists: {}",
                path_src.to_string_lossy()
            );
        }

        // clone
        let mut cmd = Command::new("git");
        cmd.arg("clone")
            .arg(
                self.path
                    .as_os_str()
                    .to_str()
                    .ok_or_else(|| anyhow!("invalid path: {}", path_src.to_string_lossy()))?,
            )
            .arg(path_src);

        let output = cmd.output().context(format!(
            "clone from {:?} to {:?} failed",
            self.path, path_src
        ))?;
        if !output.status.success() {
            bail!("clone failed {:?}", output.stderr);
        }

        // checkout
        let mut cmd = Command::new("git");
        cmd.arg("checkout");
        cmd.arg(&self.commit);
        cmd.current_dir(path_src);

        let output = cmd.output().context(format!(
            "checkout to commit {} failed in path {:?}",
            self.commit,
            path_src
        ))?;
        if !output.status.success() {
            bail!("checkout failed {:?}", output.stderr);
        }

        // done
        Ok(())
    }
}

//---------------------------------------------------------------------------------------------------//

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use rusmart_utils::config::WKS;

    /// Test case to check if a new GitRepo instance can be created successfully.
    /// Note that the workspace is a git repo so the test should pass.
    #[test]
    fn test_git_repo_new() {
        let path = WKS.base.clone();
        let repo = GitRepo::new(path, None);
        assert!(repo.is_ok());
    }

    /// Test case to check that a new GitRepo instance is not created successfully if the path is invalid.
    /// The path is invalid because the directory does not exist. This error is spawn with the ? operator.
    #[test]
    fn test_git_repo_new_invalid_path() {
        let path = PathBuf::from("invalid_path");
        let repo = GitRepo::new(path.clone(), None);
        assert!(repo.is_err());
        assert!(repo.err().unwrap().to_string().contains(&format!(
            "The issue might be because the path {:?} does not exist.",
            path
        )));
    }

    /// Test case to check that a new GitRepo instance is not created successfully if the path exists but is not a git repo.
    /// The path is invalid because the root directory does not contain a git repository.
    #[test]
    fn test_git_repo_new_invalid_repo() {
        let path = PathBuf::from("/");
        let repo = GitRepo::new(path, None);
        assert!(repo.is_err());
        assert!(repo
            .err()
            .unwrap()
            .to_string()
            .contains("commit probing failed"));
    }

    /// Test case to verify that the commit hash is returned correctly.
    /// the gitrepo that is succesful should not have an empty commit.
    #[test]
    fn test_git_repo_commit() {
        let path = WKS.base.clone();
        let repo = GitRepo::new(path, None).unwrap();
        assert!(!repo.commit().is_empty());
    }

    /// Test case for the checkout function.
    /// Checks if the repository can be checked out to a non-existent directory, which should pass.
    #[test]
    fn test_git_repo_checkout() {
        // get the path to the workspace
        let path = WKS.base.clone();
        let mut repo = GitRepo::new(path, None).unwrap();

        let result = repo.checkout(Path::new("temp_checkout"));
        assert!(result.is_ok());

        // Cleanup
        fs::remove_dir_all("temp_checkout").unwrap();
    }

    /// Test case for attempting to checkout to an existing directory, which should fail.
    #[test]
    fn test_checkout_existing_directory() {
        let path = WKS.base.clone();
        let mut repo = GitRepo::new(path.clone(), None).unwrap();

        // Create a dummy directory
        let temp_dir = PathBuf::from("temp_existing_dir");
        fs::create_dir(&temp_dir).unwrap();

        let result = repo.checkout(&temp_dir);
        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "checkout path already exists: temp_existing_dir"
        );

        // Cleanup
        fs::remove_dir_all(temp_dir).unwrap();
    }
}
