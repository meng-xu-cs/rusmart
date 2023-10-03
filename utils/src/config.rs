use std::env;
use std::fmt::{Display, Formatter};

use lazy_static::lazy_static;
use simplelog::{ColorChoice, Config, LevelFilter, TermLogger, TerminalMode};

/// Name of project
pub static PROJECT: &str = "RUSMART";

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

/// initialize all configs
pub fn initialize() {
    // logging
    let level = match *MODE {
        Mode::Prod => LevelFilter::Warn,
        Mode::Dev => LevelFilter::Info,
        Mode::Debug => LevelFilter::Debug,
        Mode::Verbose => LevelFilter::Trace,
    };
    TermLogger::init(
        level,
        Config::default(),
        TerminalMode::Mixed,
        ColorChoice::Auto,
    )
    .expect("logging facility should be initialized");
}
