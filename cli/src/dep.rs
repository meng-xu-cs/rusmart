//! # Dependency
//!
//! The `Dependency` trait is used to mark a dependency in the project. It has three functions:
//!
//! - `repo_path_from_root`: This function returns the location of the git repo from the project root.
//! - `list_configurations`: This function lists configurable options for building.
//! - `build`: This function builds the dependency from scratch.
//!
//! The `Artifact` struct is a marker over a path indicating that this is an artifact of a dependency. It has three fields:
//!
//! - `base`: The base directory.
//! - `src`: The source directory which is a subdirectory of the base directory.
//! - `dst`: The destination directory which is a subdirectory of the base directory.
//!
//! The `Scratch` struct represents the build-from-scratch state. It has three fields:
//!
//! - `repo`: The git repo.
//! - `artifact`: The path to the artifact where the source and destination directories will be created.
//! - `_phantom`: A PhantomData. This is used to differentiate between the functions of CV5 and Z3.
//!
//! The `Package` struct represents the package-ready state. It has three fields:
//!
//! - `repo`: The git repo.
//! - `artifact`: The artifact.
//! - `_phantom`: A PhantomData.
//!
//! The `DepState` enum automatically differentiates the scratch and package version of LLVM. It has two variants:
//!
//! - `Scratch`: The scratch state.
//! - `Package`: The package state.
//!
//! The `DepState` enum has three functions:
//!
//! - `new`: Get the deps state.
//! - `list_configurations`: List the possible build options.
//! - `build`: Build the package.

//-------------------------------------------------------------------------------------------//
// the fs module is used to interact with the filesystem for example to create, read, update and delete files
use std::fs;
// the PhantomData is used to indicate that the type parameter is used in the struct but not stored
// This is handy to check the type and lifetime of the type parameter at compile time
use std::marker::PhantomData;
// the Path and PathBuf are used to represent file paths (first is immutable, second is mutable)
use std::path::{Path, PathBuf};
// the anyhow module is used to handle errors in a more convenient way
// Result is a type that represents either success or failure and is a synonym for Result<T, Error> of the std::result module
use anyhow::{bail, Result};
// a logging facade that provides macros for logging at different levels
// info! is used to log information messages
// warn! is used to log warning messages (not necessarily errors)
use log::{info, warn};
// the tempfile module is used to create temporary files and directories
use tempfile::tempdir;
// the workspace struct containing the base and studio paths
use rusmart_utils::config::WKS;
// the git repo struct containing the path and commit hash
use crate::git::GitRepo;

//-----------------------------------------------------------------------------------------------//

/// A trait that marks a dependency in the project
/// The only dependencies we currently have are Z3 and CVC5
pub trait Dependency {
    /// Location of the git repo from the project root
    /// either deps/z3 or deps/cvc5
    fn repo_path_from_root() -> &'static [&'static str];

    /// List configurable options for building
    fn list_configurations(artifact: &Artifact) -> Result<()>;

    /// Build the dependency from scratch
    fn build(artifact: &Artifact) -> Result<()>;
}

//-----------------------------------------------------------------------------------------------//

/// A marker over a path indicating that this is an artifact of a dependency
#[derive(Debug, PartialEq, Eq)]
pub struct Artifact {
    /// The base directory
    base: PathBuf,
    /// The source directory which is a subdirectory of the base directory
    pub src: PathBuf,
    /// The destination directory which is a subdirectory of the base directory
    pub dst: PathBuf,
}

/// Implement the Artifact struct
impl Artifact {
    /// Initialize an artifact from a git repo
    ///
    /// # Arguments
    ///
    /// * `repo` - A mutable reference to a GitRepo
    /// * `base` - A PathBuf
    ///
    /// # Returns
    ///
    /// * `Result<Self>` - A result containing the Artifact struct
    /// The artifact struct contains the base, source and destination paths
    /// -- base -- src (this is a git repo)
    ///     |
    ///     ------ dst (this is empty until the build is done)
    pub fn init(repo: &mut GitRepo, base: PathBuf) -> Result<Self> {
        if base.exists() {
            bail!("artifact path already exists");
        }
        fs::create_dir_all(&base)?;

        let src = base.join("src");
        repo.checkout(&src)?;
        let dst = base.join("dst");
        fs::create_dir(&dst)?;

        Ok(Self { base, src, dst })
    }

    /// Try to create an artifact from a path
    ///
    /// # Arguments
    ///
    /// * `path` - A reference to a Path
    ///
    /// # Returns
    ///
    /// * `Result<Option<Self>>` - A result containing an optional Artifact struct
    /// If the path does not exist, the result will be Ok(None)
    /// If the path exists but is not a directory (for example a file), the result will be an error
    /// If the path exists and is a directory, but doesn't contain both src and dst directories, the result will be an error
    /// If the path exists and is a directory containing both src and dst directories, the result will be Ok(Some(Artifact))
    pub fn seek(path: &Path) -> Result<Option<Self>> {
        if !path.exists() {
            return Ok(None);
        }
        if !path.is_dir() {
            bail!("artifact path exists but is not a directory");
        }

        let src = path.join("src");
        let dst = path.join("dst");

        if !src.is_dir() || !dst.is_dir() {
            bail!("invalid content layout in the artifact directory");
        }
        Ok(Some(Self {
            base: path.to_path_buf(),
            src,
            dst,
        }))
    }
}

/// A struct that represents the build-from-scratch state
#[derive(Debug, PartialEq, Eq)]
pub struct Scratch<T: Dependency> {
    repo: GitRepo,
    artifact: PathBuf,
    _phantom: PhantomData<T>,
}

/// Implement the Scratch struct
impl<T: Dependency> Scratch<T> {
    /// making a dependency package from scratch
    ///
    /// # Arguments
    ///
    /// * `self` - A Scratch
    ///
    /// # Returns
    ///
    /// * `Result<Package<T>>` - A result containing the made package
    /// creates the `artifact` directory, with two subdirectories: src and dst.
    /// github repository is cloned and checked out inside the src directory. Until this step the dst directory is empty.
    /// Then the artifact is built which depending on whether it is a Z3 or CVC5 artifact the behaviour is different. This is why we have defined a PhatomData so that the corresponding function of the type can be called.
    /// After that a package is returned with the same data.
    // Not tested as the build function is computationally expensive and time-consuming.
    pub fn make(self) -> Result<Package<T>> {
        let Self {
            mut repo,
            artifact,
            _phantom,
        } = self;

        let artifact = Artifact::init(&mut repo, artifact)?;
        // build the artifact. This is the time consuming part
        T::build(&artifact)?;

        Ok(Package {
            repo,
            artifact,
            _phantom: PhantomData,
        })
    }
}

/// A struct that represents the package-ready state
/// The GitRepo and Artifact are bundled together so that they are connected for reusable build/destroys.
#[derive(Debug, PartialEq, Eq)]
pub struct Package<T: Dependency> {
    repo: GitRepo,
    artifact: Artifact,
    _phantom: PhantomData<T>,
}

impl<T: Dependency> Package<T> {
    /// Destroy the deps so that we can build it again
    ///
    /// # Arguments
    ///
    /// * `self` - A Package
    ///
    /// # Returns
    ///
    /// * `Result<Scratch<T>>` - A result containing the Scratch
    ///
    /// Converting a package to a scratch.
    /// a package has a git repo - which is the path to the git repository and the commit hash.
    /// it has an artifact - which has the base, source and destination paths. The base is the path to the artifact directory, which contains the src and dst directories. Realistically, the src directory is the git repository and the build is done when we call the make function of scratch.
    /// The destroy function removes the base directory (with the src and dst directories) and returns a scratch which points to the same git repo and has the same base path. This allows us to build the artifact from scratch again.
    /// So the destory and make functions are inverses of each other.
    pub fn destroy(self) -> Result<Scratch<T>> {
        let Self {
            repo,
            artifact,
            _phantom,
        } = self;

        fs::remove_dir_all(&artifact.base)?;

        Ok(Scratch {
            repo,
            artifact: artifact.base,
            _phantom: PhantomData,
        })
    }

    /// Expose the artifact destination
    ///
    /// # Arguments
    ///
    /// * `self` - A Package
    ///
    /// # Returns
    ///
    /// * `PathBuf` - The path to the artifact dst directory
    ///
    /// This function returns the path to the artifact dst directory.
    pub fn export(self) -> PathBuf {
        self.artifact.dst
    }
}

/// Automatically differentiate the scratch and package version of LLVM
#[derive(Debug, PartialEq, Eq)]
pub enum DepState<T: Dependency> {
    /// The scratch state
    Scratch(Scratch<T>),
    /// The package state
    Package(Package<T>),
}

impl<T: Dependency> DepState<T> {
    /// Get the deps state
    pub fn new() -> Result<Self> {
        // derive the correct path
        let segments = T::repo_path_from_root();

        let mut repo_path = WKS.base.clone();
        repo_path.extend(segments);
        let repo = GitRepo::new(repo_path, None)?;

        let mut artifact = WKS.studio.to_path_buf();
        artifact.extend(segments);
        artifact.push(repo.commit());

        // check the existence of the pre-built package
        let state = match Artifact::seek(&artifact)? {
            None => Self::Scratch(Scratch {
                repo,
                artifact,
                _phantom: PhantomData,
            }),
            Some(artifact) => Self::Package(Package {
                repo,
                artifact,
                _phantom: PhantomData,
            }),
        };

        // done
        Ok(state)
    }

    /// List the possible build options
    pub fn list_configurations(self) -> Result<()> {
        let mut repo = match self {
            Self::Scratch(Scratch { repo, .. }) => repo,
            Self::Package(Package { repo, .. }) => repo,
        };

        // always happens in tmpfs
        let tmp = tempdir()?;
        let artifact = Artifact::init(&mut repo, tmp.path().join("artifact"))?;
        T::list_configurations(&artifact)?;

        // done
        tmp.close()?;
        Ok(())
    }

    /// Build the package
    // may be computationally expensive (is not tested)
    pub fn build(self, force: bool) -> Result<()> {
        let scratch = match self {
            Self::Scratch(scratch) => scratch,
            Self::Package(package) => {
                if !force {
                    info!("Package already exists");
                    return Ok(());
                } else {
                    warn!("Force rebuilding package");
                    package.destroy()?
                }
            }
        };
        scratch.make()?;
        Ok(())
    }
}

// ---------------------------------------TESTS---------------------------------------//

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dep_cvc5::DepCVC5;
    use crate::dep_z3::DepZ3;
    use std::process::Command;

    /// Building the git submodules and returning the paths to the modules
    /// This helper function runs the `git submodule update --init --recursive` command in the workspace root.
    /// This is necessary to clone the z3 repository to the deps/z3 directory and cvc5 repo to the deps/cvc5 path.
    pub fn setup() -> (PathBuf, PathBuf) {
        // getting the path to the workspace directory
        let path = &WKS.base;

        // running the command to build the submodules
        let mut command = Command::new("git");
        command
            .args(["submodule", "update", "--init", "--recursive"])
            .current_dir(path.clone());

        let output = command.output().expect("could not update submodules...");
        if !output.status.success() {
            panic!("could not update submodules defined at .gitmodules file...");
        }

        let z3_path = path.join(DepZ3::repo_path_from_root().join("/"));
        let cvc5_path = path.join(DepCVC5::repo_path_from_root().join("/"));

        (z3_path, cvc5_path)
    }

    #[test]
    /// This test should be successful given that deps/z3 directory exists and is a git repository.
    /// The tmp directory is created from the cli crate; inside of which two directories src and dst are created.
    /// inside of src the github repository is cloned and checked out.
    fn test_artifact_init() {
        let (z3_path, _) = setup();
        let mut repo = GitRepo::new(z3_path, None).expect("could not create git repo");

        let mut base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        base = base.join("tmp");
        let artifact = Artifact::init(&mut repo, base.clone());

        assert!(artifact.is_ok());

        // clean up
        fs::remove_dir_all(&base).expect("could not remove tmp directory");
    }

    #[test]
    /// This should fail given that the directory temp exists before initializing the artifact.
    fn test_artifact_init_fail() {
        let (z3_path, _) = setup();
        let mut repo = GitRepo::new(z3_path, None).expect("could not create git repo");

        let mut base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        base = base.join("tmp");
        fs::create_dir_all(&base).expect("could not create tmp directory");

        let artifact = Artifact::init(&mut repo, base.clone());

        assert!(artifact.is_err());
        assert_eq!(
            artifact.err().expect("could not get error value").to_string(),
            "artifact path already exists"
        );

        // clean up
        fs::remove_dir_all(&base).expect("could not remove tmp directory");
    }

    #[test]
    /// The tmp directory does not exist so the result will be Ok(None).
    fn test_artifact_seek_path_not_exist() {
        let tmp = PathBuf::from("tmp");

        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path = path.join(tmp);

        let artifact = Artifact::seek(&path).expect("could not seek artifact");
        assert_eq!(artifact, None);
    }

    #[test]
    /// Here we test the seek function when the path exists but is not a directory.
    fn test_artifact_seek_path_not_dir() {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path = path.join("src/cli.rs");
        let artifact = Artifact::seek(&path);

        assert!(artifact
            .is_err_and(|x| { x.to_string() == "artifact path exists but is not a directory" }));
    }

    #[test]
    /// Here we test that either the directory src or dst do not exist in the given path
    fn test_artifact_seek_invalid_layout() {
        let path = PathBuf::from("./src");
        let artifact = Artifact::seek(&path);

        assert!(artifact.is_err_and(|x| {
            x.to_string() == "invalid content layout in the artifact directory"
        }));
    }

    #[test]
    /// This tests the seek function of the artifact struct when the path exists, is a directory, and contains both src and dst directories.
    fn test_artifact_seek_ok() {
        let (z3_path, _) = setup();
        let mut repo = GitRepo::new(z3_path, None).expect("could not create git repo");

        let mut base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        base = base.join("tmp");
        let _ = Artifact::init(&mut repo, base.clone());

        if let Ok(Some(artifact)) = Artifact::seek(&base) {
            assert_eq!(artifact.base, base);
            assert_eq!(artifact.dst, base.join("dst"));
            assert_eq!(artifact.src, base.join("src"));
        };

        // clean up
        fs::remove_dir_all(&base).expect("could not remove tmp directory");
    }

    /// Helper function to create a package
    fn make_package() -> (PathBuf, Package<DepZ3>) {
        let (z3_path, _) = setup();
        let mut repo = GitRepo::new(z3_path, None).expect("could not create git repo");

        let mut base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        base = base.join("tmp");
        let artifact = Artifact::init(&mut repo, base.clone()).expect("could not initialize artifact");

        (
            base,
            Package {
                repo,
                artifact,
                _phantom: PhantomData,
            },
        )
    }

    #[test]
    /// This test should be successful given that the package is destroyed and the base directory is removed.
    fn test_package_destroy() {
        let (_, package) = make_package();
        let scratch = package.destroy().expect("could not destroy package");

        assert!(!scratch.artifact.exists());
    }

    #[test]
    /// The export function should return the path to the artifact dst directory.
    fn test_package_export() {
        let (base, package) = make_package();
        let path = package.export();

        assert!(path.exists());
        assert_eq!(path, base.join("dst"));

        // clean up
        fs::remove_dir_all(&base).expect("could not remove tmp directory");
    }

    #[test]
    /// Testing the new function of the DepState enum for the Z3 dependency
    fn test_new_depstate() {
        let (z3_path, _) = setup();
        let git_repo = GitRepo::new(z3_path, None).expect("could not create git repo");

        let dep_state = DepState::<DepZ3>::new().expect("could not create new dep state");

        let path_wk_studio_native_deps_z3_commit = WKS
            .base
            .join("studio/native")
            .join(DepZ3::repo_path_from_root().join("/"))
            .join(git_repo.commit());

        if path_wk_studio_native_deps_z3_commit.exists() {
            assert_eq!(
                dep_state,
                DepState::Package(Package {
                    repo: git_repo,
                    artifact: Artifact {
                        base: path_wk_studio_native_deps_z3_commit.clone(),
                        src: path_wk_studio_native_deps_z3_commit.join("src"),
                        dst: path_wk_studio_native_deps_z3_commit.join("dst")
                    },
                    _phantom: PhantomData::<DepZ3>
                })
            )
        } else {
            assert_eq!(
                dep_state,
                DepState::Scratch(Scratch {
                    repo: git_repo,
                    artifact: path_wk_studio_native_deps_z3_commit,
                    _phantom: PhantomData::<DepZ3>
                })
            )
        }
    }

    #[test]
    /// testing the list configurations function of the DepState enum for the Z3 dependency
    fn test_list_configurations_depstate() {
        let depstate = DepState::<DepZ3>::new().expect("could not create new dep state");
        let res = depstate.list_configurations();

        assert!(res.is_ok());
    }
}
