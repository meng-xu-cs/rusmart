use std::ffi::OsStr;
use std::fmt::{Display, Formatter};
use std::fs::Permissions;
use std::io::Read;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::{Duration, SystemTime};
use std::{fs, thread};

use command_group::CommandGroup;
use itertools::Itertools;
use log::{debug, warn};

/// Execution timeout
const BACKEND_TIMEOUT: Duration = Duration::from_secs(60 * 10);

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

/// Utility on force coercion of OsStr to str
fn to_ascii(v: &OsStr) -> &str {
    v.to_str().expect("ascii string")
}

/// Utility on command to shell script conversion
pub fn mk_shell_script(command: &Command, path: &Path) {
    // produce the shell command
    let shell = format!(
        "{} {} {}",
        // environment
        command.get_envs().format_with(" ", |(k, v), f| {
            f(&format_args!("{}={}", to_ascii(k), v.map_or("", to_ascii),))
        }),
        // program
        to_ascii(command.get_program()),
        // arguments
        command.get_args().map(to_ascii).format(" "),
    );

    // write to file
    fs::write(path, shell.trim()).expect("file IO");

    // change file permission
    fs::set_permissions(path, Permissions::from_mode(0o755)).expect("file permissions");
}
