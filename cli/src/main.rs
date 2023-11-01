use anyhow::Result;
use structopt::StructOpt;

use rusmart_cli::cli::DepArgs;

#[derive(StructOpt)]
#[structopt(
    name = "semantic-smt-cli",
    about = "A command line interface for the semantic-smt project",
    rename_all = "kebab-case"
)]
struct Args {
    /// Subcommand
    #[structopt(subcommand)]
    command: Command,
}

#[derive(StructOpt)]
enum Command {
    /// The dependencies
    #[structopt(name = "deps")]
    Deps(DepArgs),
}

/// Main entrypoint
pub fn main() -> Result<()> {
    let args = Args::from_args();
    let Args { command } = args;

    // run the command
    match command {
        Command::Deps(sub) => sub.run()?,
    }
    Ok(())
}
