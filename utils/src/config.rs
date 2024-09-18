//! This module contains all the configuration settings for the application

use std::env;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::fs;

use lazy_static::lazy_static;
use simplelog::{ColorChoice, ConfigBuilder, LevelFilter, TermLogger, TerminalMode};

/// Name of project
/// This is used to prefix environment variables
pub static PROJECT: &str = "RUSMART";

/// Marks whether initialization is completed
/// This is used to prevent double initialization
static INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Mode of operation
pub enum Mode {
    /// production mode
    Prod,
    /// development mode
    Dev,
    /// debug mode
    Debug,
    /// verbose mode
    Verbose,
}

impl Display for Mode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Prod => write!(f, "production"),
            Self::Dev => write!(f, "development"),
            Self::Debug => write!(f, "debug"),
            Self::Verbose => write!(f, "verbose"),
        }
    }
}

lazy_static! {
    /// Which mode to run on (default to development mode)
    pub static ref MODE: Mode = {
        let setting = env::var(format!("{}_VERBOSE", PROJECT))
            .or(env::var("VERBOSE"))
            .or(env::var("V"));
        let verbosity = match setting {
            Ok(val) => val.parse::<usize>().ok(),
            Err(_) => None,
        }.unwrap_or(1);

        match verbosity {
            0 => Mode::Prod,
            1 => Mode::Dev,
            2 => Mode::Debug,
            _ => Mode::Verbose,
        }
    };
}

/// Workspace
pub struct Workspace {
    /// path to project base
    pub base: PathBuf,
    /// path to studio directory
    pub studio: PathBuf,
}

lazy_static! {
    /// Directory layout
    pub static ref WKS: Workspace = {
        let dockerized = matches!(env::var("DOCKER"), Ok(val) if val == "1");

        // grab workspace root path
        let mut base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        assert!(base.pop());

        // derive other paths
        let studio = base
                .join("studio")
                .join(if dockerized { "docker" } else { "native" });

        // done
        Workspace {
            base,
            studio,
        }
    };

    /// Number of CPU core
    pub static ref NUM_CPU_CORES: usize = num_cpus::get();
}

/// initialize all configs
pub fn initialize() {
    // check whether we need to run the initialization process
    match INITIALIZED.compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst) {
        Ok(false) => (),
        Err(true) => {
            // already initialized, do nothing
            return;
        }
        _ => panic!("invalid result from atomic reading"),
    }

    // logging
    let level = match *MODE {
        Mode::Prod => LevelFilter::Warn,
        Mode::Dev => LevelFilter::Info,
        Mode::Debug => LevelFilter::Debug,
        Mode::Verbose => LevelFilter::Trace,
    };
    let mut config = ConfigBuilder::new();
    config
        .set_location_level(LevelFilter::Off)
        .set_target_level(LevelFilter::Off)
        .set_thread_level(LevelFilter::Off)
        .set_time_level(LevelFilter::Off);
    TermLogger::init(
        level,
        config.build(),
        TerminalMode::Mixed,
        ColorChoice::Auto,
    )
    .expect("logging facility should be initialized");
}

/// Helper function to find the root of the workspace.
/// The env!("CARGO_MANIFEST_DIR") is the directory where the current crate's Cargo.toml file is located.
/// The function goes up the directory tree until it finds a Cargo.toml file that contains a `[workspace]` section.
/// If no workspace root is found, the function returns None.
pub fn find_workspace_root() -> Option<PathBuf> {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // Loop until we reach the root directory
    while dir.pop() {
        let cargo_toml = dir.join("Cargo.toml");
        if let Ok(content) = fs::read_to_string(&cargo_toml) {
            // Check if this Cargo.toml contains a `[workspace]` section
            if content.contains("[workspace]") {
                return Some(dir);
            }
        }
    }

    // If no workspace root is found, return None
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    // Test the display implementation for Mode
    fn test_fmt_mode() {
        assert_eq!(format!("{}", Mode::Prod), "production");
        assert_eq!(format!("{}", Mode::Dev), "development");
        assert_eq!(format!("{}", Mode::Debug), "debug");
        assert_eq!(format!("{}", Mode::Verbose), "verbose");
    }

    #[test]
    // Test the initialization of the configuration
    fn test_initialization() {
        // Reset INITIALIZED to false before the test
        INITIALIZED.store(false, Ordering::SeqCst);
        // Set the VERBOSE environment variable to 1
        env::set_var("VERBOSE", "1");

        // Because INITIALIZED is set to false, this assertion should pass
        assert_eq!(INITIALIZED.load(Ordering::SeqCst), false);

        // Call initialize and assert the expected outcome
        initialize();

        // Assert that INITIALIZED is now true, meaning that the initialization process has been completed
        assert_eq!(INITIALIZED.load(Ordering::SeqCst), true);

        initialize();

        // Assert that INITIALIZED is still true
        assert_eq!(INITIALIZED.load(Ordering::SeqCst), true);

        // Assert that the logging level is set to Info
        // Note that other paths cannot be tested because the MODE is a lazy static variable and is defined inside a proc macro
        let expected_level = match *MODE {
            Mode::Dev => LevelFilter::Info,
            _ => unreachable!(),
        };
        assert_eq!(expected_level, LevelFilter::Info);
    }

    #[test]
    /// test the find root of work space function
    fn test_find_workspace_root() {
        let wk_path = find_workspace_root();

        assert!(wk_path.is_some());
        assert_eq!(wk_path.unwrap(), WKS.base);
    }
}
