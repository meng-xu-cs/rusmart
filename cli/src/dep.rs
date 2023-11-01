use std::fs;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};

use anyhow::{bail, Result};
use log::{info, warn};
use rusmart_utils::config::WKS;
use tempfile::tempdir;

use crate::git::GitRepo;

/// A marker over a path indicating that this is an artifact of a dependency
pub struct Artifact {
    base: PathBuf,
    pub src: PathBuf,
    pub dst: PathBuf,
}

impl Artifact {
    /// Initialize an artifact without having any content in it
    fn init(repo: &mut GitRepo, base: PathBuf) -> Result<Self> {
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

/// A trait that marks a dependency in the project
pub trait Dependency {
    /// Location of the git repo from the project root
    fn repo_path_from_root() -> &'static [&'static str];

    /// List configurable options for building
    fn list_configurations(artifact: &Artifact) -> Result<()>;

    /// Build the dependency from scratch
    fn build(artifact: &Artifact) -> Result<()>;
}

/// A struct that represents the build-from-scratch state
pub struct Scratch<T: Dependency> {
    repo: GitRepo,
    artifact: PathBuf,
    _phantom: PhantomData<T>,
}

impl<T: Dependency> Scratch<T> {
    /// Build the deps from scratch
    pub fn make(self) -> Result<Package<T>> {
        let Self {
            mut repo,
            artifact,
            _phantom,
        } = self;

        let artifact = Artifact::init(&mut repo, artifact)?;
        T::build(&artifact)?;

        Ok(Package {
            repo,
            artifact,
            _phantom: PhantomData,
        })
    }
}

/// A struct that represents the package-ready state
pub struct Package<T: Dependency> {
    repo: GitRepo,
    artifact: Artifact,
    _phantom: PhantomData<T>,
}

impl<T: Dependency> Package<T> {
    /// Destroy the deps so that we can build it again
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
    pub fn export(self) -> PathBuf {
        self.artifact.dst
    }
}

/// Automatically differentiate the scratch and package version of LLVM
pub enum DepState<T: Dependency> {
    Scratch(Scratch<T>),
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
    pub fn build(self, force: bool) -> Result<()> {
        let scratch = match self {
            DepState::Scratch(scratch) => scratch,
            DepState::Package(package) => {
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
