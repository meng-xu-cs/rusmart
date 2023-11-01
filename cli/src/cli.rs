use std::path::PathBuf;

use anyhow::{bail, Result};
use structopt::StructOpt;

use crate::dep::{DepState, Dependency};
use crate::dep_cvc5::DepCVC5;
use crate::dep_z3::DepZ3;

#[derive(StructOpt)]
pub enum DepAction {
    /// Config the dependency
    Config,

    /// Build the dependency
    Build {
        /// Force the build to proceed
        #[structopt(short, long)]
        force: bool,
    },
}

#[derive(StructOpt)]
pub struct DepArgs {
    /// Name of the deps
    name: String,

    /// Subcommand
    #[structopt(subcommand)]
    action: DepAction,
}

impl DepArgs {
    fn run_internal<T: Dependency>(self) -> Result<()> {
        let Self {
            name: _,
            action: command,
        } = self;
        let state: DepState<T> = DepState::new()?;

        match command {
            DepAction::Config => state.list_configurations()?,
            DepAction::Build { force } => state.build(force)?,
        }
        Ok(())
    }

    pub fn run(self) -> Result<()> {
        let name = self.name.as_str();
        match name {
            "z3" => self.run_internal::<DepZ3>(),
            "cvc5" => self.run_internal::<DepCVC5>(),
            _ => bail!("Invalid deps name: {}", name),
        }
    }
}

/// Get the path to an artifact or return an error
fn artifact<T: Dependency>() -> Result<PathBuf> {
    let pkg = match DepState::<T>::new()? {
        DepState::Package(package) => package,
        DepState::Scratch(scratch) => scratch.make()?,
    };
    Ok(pkg.export())
}

/// Get the path to an artifact, panics if unable to obtain the path
fn expect_artifact<T: Dependency>() -> PathBuf {
    match artifact::<T>() {
        Ok(r) => r,
        Err(e) => panic!("unexpected error: {}", e),
    }
}

/// Force to retrieve the path to z3
pub fn expect_z3() -> PathBuf {
    expect_artifact::<DepZ3>()
}

/// Force to retrieve the path to cvc5
pub fn expect_cvc5() -> PathBuf {
    expect_artifact::<DepCVC5>()
}
