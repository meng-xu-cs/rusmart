use std::fs;

use anyhow::Result;
use clap::{Parser, Subcommand};

use rusmart_cli::cli::DepArgs;
use rusmart_utils::config::{initialize, WKS};

#[derive(Parser)]
#[clap(
    name = "semantic-smt-cli",
    about = "A command line interface for the Rusmart project",
    rename_all = "kebab-case"
)]
struct Args {
    /// Subcommand
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Wipe-clean the entire workspace
    Reset,

    /// Dependencies
    #[command(subcommand)]
    Deps(DepArgs),
}

/// Main entrypoint
pub fn main() -> Result<()> {
    initialize();

    // parse arguments
    let args = Args::parse();
    let Args { command } = args;

    // run the command
    match command {
        Command::Reset => fs::remove_dir_all(&WKS.studio)?,
        Command::Deps(sub) => sub.run()?,
    }
    Ok(())
}
