//! # package rusmart_cli
//! ## binary crate: This crate provides the command-line interface for the Rusmart project.
//!
//! - The entry point is the `main` function, which parses command-line arguments and executes the corresponding actions.
//! - The CLI uses the `clap` crate for argument parsing and the `rusmart_utils` crate for workspace initialization.
//!

// ------------------------------------------------------------------------------------------------//

// the Result type is a type alias for `Result<T, anyhow::Error>` for simplified error handling.
use anyhow::{Context, Result};
// the `clap` crate provides a simple way to parse command-line arguments.
// Parser is a trait in the clap crate that allows for parsing command-line arguments into a struct.
use clap::Parser;
// Import the `Command` struct from the `rusmart_cli` library crate to handle command management.
use rusmart_cli::cli::Command;
// Import the `initialize` function and `WKS` constant from the `rusmart_utils` crate.
use rusmart_utils::config;
use rusmart_utils::config::WKS;
// handling the file system operations like creating, deleting, and reading files.
use std::fs;

// ------------------------------------------------------------------------------------------------//

// By deriving Parser, command-line arguments are automatically parsed into the fields of the Args struct.
#[derive(Parser)]
/// * Semantic SMT CLI
/// By running the cargo run -- --help command, the following help message is displayed:
/// Tool: semantic-smt-cli
/// Author: Meng Xu <meng.xu.cs@uwaterloo.ca>
/// Version: 0.1.0
/// A command line interface for the Rusmart project
/// Usage: rusmart-cli <COMMAND>
/// Commands:
/// reset  Wipe-clean the entire workspacedksvjdfsjkn
/// deps   Manage dependencies (subcommand is defined in DepArgs)
/// help   Print this message or the help of the given subcommand(s)
/// Options:
/// -h, --help     Print help
/// -V, --version  Print version
/// The about, version, and author attributes are taken from the Cargo.toml file.
/// The rename_all = "kebab-case" attribute converts the struct field names to kebab-case.
/// The help_template attribute customizes the help message format.
/// In #[command(subcommand)], the Subcommand trait is used to define subcommands within your command-line interface. Subcommands allow having different functionalities under a main command, similar to how git works (e.g., git add, git commit).
#[command(
    name = "semantic-smt-cli",
    about,
    version,
    author,
    rename_all = "kebab-case"
)]
#[command(
    help_template = "Tool: {name}\nAuthor: {author-with-newline}Version: {version}{about-section}\n{usage-heading} {usage} \n {all-args} {tab}"
)]
struct Args {
    /// Subcommand
    #[command(subcommand)]
    command: Command,
}

/// Main entry point of the CLI program.
/// This function initializes the workspace configuration, parses the CLI arguments,
/// and matches the provided command to execute the corresponding action.
///
/// # Errors
/// If the workspace can't be reset or dependencies can't be managed, it returns an `anyhow::Error`.
// not part of the code coverage
#[cfg(not(feature = "tarpaulin_include"))]
pub fn main() -> Result<()> {
    // Initialize workspace
    config::initialize();

    // Parse command-line arguments into the `Args` struct
    let args = Args::parse();
    let Args { command } = args;

    // Match the provided command and execute the appropriate functionality
    match command {
        // For the 'reset' command, remove the studio directory
        Command::Reset => fs::remove_dir_all(&WKS.studio)
            .context("The studio directory does not exist; there is nothing to reset...")?,

        // For the 'deps' command, delegate to the subcommand handler
        // This runs the subcommand which will in turn internally run the corresponding dependency action.
        // This creates a new DepState and lists the configurations or builds the dependency based on the subcommand.
        Command::Deps(sub) => sub.run()?,
    }

    // Indicate success
    Ok(())
}
