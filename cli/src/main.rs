use std::fs;

use anyhow::Result;
use structopt::StructOpt;

use rusmart_cli::cli::DepArgs;
use rusmart_utils::config::{initialize, WKS};

#[derive(StructOpt)]
#[structopt(
    name = "semantic-smt-cli",
    about = "A command line interface for the Rusmart project",
    rename_all = "kebab-case"
)]
struct Args {
    /// Subcommand
    #[structopt(subcommand)]
    command: Command,
}

#[derive(StructOpt)]
enum Command {
    /// Wipe-clean the entire workspace
    Reset,
    /// Dependencies
    #[structopt(name = "deps")]
    Deps(DepArgs),
}

/// Main entrypoint
pub fn main() -> Result<()> {
    initialize();

    // parse arguments
    let args = Args::from_args();
    let Args { command } = args;

    // run the command
    match command {
        Command::Reset => fs::remove_dir_all(&WKS.studio)?,
        Command::Deps(sub) => sub.run()?,
    }
    Ok(())
}
