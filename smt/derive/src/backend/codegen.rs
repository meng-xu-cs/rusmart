use std::fmt::{Display, Formatter};
use std::io::Read;
use std::path::Path;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, SystemTime};

use command_group::CommandGroup;
use log::{debug, warn};

use crate::backend::error::BackendResult;
use crate::ir::ctxt::IRContext;

/// Generic trait for backend code generator
pub trait CodeGen {
    /// Mark the name of the code generator
    fn name(&self) -> String;

    /// Execute the IR with backend-specific logic
    fn execute(&self, ir: &IRContext, path_wks: &Path) -> BackendResult<Response>;
}

/// A utility for source code builder
pub struct ContentBuilder {
    buffer: String,
    indent: usize,
}

impl ContentBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
            indent: 0,
        }
    }

    /// Add a new line to the content
    pub fn line<S: AsRef<str>>(&mut self, code: S) {
        for _ in 0..self.indent {
            self.buffer.push('\t');
        }
        self.buffer.push_str(code.as_ref());
        self.buffer.push('\n');
    }

    /// Indented builder
    pub fn scope<F: Fn(&mut Self)>(&mut self, f: F) {
        self.indent += 1;
        f(self);
        self.indent -= 1;
    }

    /// Convert it to string
    pub fn build(self) -> String {
        self.buffer
    }
}

/// Solving result
pub enum Response {
    /// solver does not return anything
    Timeout,
    /// solver explicitly returns the unknown status
    Unknown,
    /// SMT: sat
    Sat,
    /// SMT: unsat
    Unsat,
}

impl Display for Response {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            Self::Timeout => "timeout",
            Self::Unknown => "unknown",
            Self::Sat => "sat",
            Self::Unsat => "unsat",
        };
        f.write_str(text)
    }
}

/// Execution timeout
const BACKEND_TIMEOUT: Duration = Duration::from_secs(60 * 10);

/// Utility on backend invocation and response conversion
pub fn run_backend(mut command: Command) -> Response {
    // launch the command
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    let mut child = command
        .group_spawn()
        .expect("spawning child group for command");

    let mut stdout = child.inner().stdout.take().expect("piped stdout");
    let mut stderr = child.inner().stderr.take().expect("piped stderr");
    let timestamp = SystemTime::now();

    // monitor the execution
    let thread = thread::spawn(move || {
        loop {
            // print any on-going messages
            let mut message = String::new();
            stderr.read_to_string(&mut message).expect("reading stderr");
            if !message.is_empty() {
                debug!("{}", message);
            }

            // check status
            if let Ok(Some(status)) = child.try_wait() {
                // print any remaining messages
                let mut message = String::new();
                stderr.read_to_string(&mut message).expect("reading stderr");
                if !message.is_empty() {
                    debug!("{}", message);
                }
                return Some(status);
            }

            // check timeout
            if timestamp.elapsed().expect("time measurement") > BACKEND_TIMEOUT {
                child
                    .kill()
                    .expect("terminate the entire child process group");
                return None;
            }

            // wait a bit longer
            thread::sleep(Duration::from_millis(200));
        }
    });

    // wait for thread to finish
    let status = thread.join().expect("monitoring thread completed");

    // read stdout first
    let mut output = String::new();
    stdout.read_to_string(&mut output).expect("reading stdout");

    // resolve the final response
    let response = match status {
        None => {
            if !output.is_empty() {
                warn!("output received from a timeout execution: {}", output);
            }
            Response::Timeout
        }
        Some(e) => {
            if !e.success() {
                if !output.is_empty() {
                    warn!("output received from a crashed execution: {}", output);
                }
                panic!("backend execution crashed with status {}", e);
            }
            match output.as_str() {
                "unknown" => Response::Unknown,
                "sat" => Response::Sat,
                "unsat" => Response::Unsat,
                _ => panic!("invalid response: {}", output),
            }
        }
    };
    response
}
