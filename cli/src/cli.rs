use std::path::PathBuf;

use anyhow::Result;
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

impl DepAction {
    fn run_internal<T: Dependency>(self) -> Result<()> {
        let state: DepState<T> = DepState::new()?;
        match self {
            DepAction::Config => state.list_configurations()?,
            DepAction::Build { force } => state.build(force)?,
        }
        Ok(())
    }
}

#[derive(StructOpt)]
#[allow(clippy::upper_case_acronyms)]
pub enum DepArgs {
    /// Solver Z3
    Z3(DepAction),
    /// Solver CVC5
    CVC5(DepAction),
}

impl DepArgs {
    pub fn run(self) -> Result<()> {
        match self {
            Self::Z3(action) => action.run_internal::<DepZ3>(),
            Self::CVC5(action) => action.run_internal::<DepCVC5>(),
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
