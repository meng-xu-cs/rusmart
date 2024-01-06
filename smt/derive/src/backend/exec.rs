use std::fmt::{Display, Formatter};
use std::io::Read;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::{Duration, SystemTime};
use std::{fs, thread};

use command_group::CommandGroup;
use log::{debug, warn};

use rusmart_utils::config::{Mode, MODE};

use crate::backend::codegen::CodeGen;
use crate::backend::error::BackendResult;
use crate::ir::ctxt::IRContext;

/// Execution timeout
const MAIN_EXECUTABLE: &str = "main";

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

/// Unified backend generation and execution service
pub fn invoke_backend(
    ir: &IRContext,
    backend: &dyn CodeGen,
    path_wks: &Path,
) -> BackendResult<Response> {
    // generate code
    let code = backend.process(ir)?;

    // save source code to designated file
    let path_src = path_wks.join(format!("{}.{}", MAIN_EXECUTABLE, backend.flavor()));
    if path_src.exists() {
        panic!("source file already exists");
    }
    fs::write(&path_src, code).unwrap_or_else(|e| panic!("IO error on source file: {}", e));

    // generate and save cmake file
    let path_cmake = path_wks.join("CMakeLists.txt");
    fs::write(path_cmake, backend.cmake())
        .unwrap_or_else(|e| panic!("IO error on cmake file: {}", e));

    // config
    let path_build = path_wks.join("build");
    fs::create_dir(&path_build).expect("create fresh build directory");

    let mut cmd = Command::new("cmake");
    cmd.arg("-G")
        .arg("Ninja")
        .arg(path_wks)
        .current_dir(&path_build);
    if matches!(*MODE, Mode::Prod | Mode::Dev) {
        cmd.stdout(Stdio::null());
        cmd.stderr(Stdio::null());
    }

    let status = cmd
        .status()
        .unwrap_or_else(|e| panic!("unexpected error in command invocation: {}", e));
    if !status.success() {
        panic!("setup failed, check output for details");
    }

    // compile
    let mut cmd = Command::new("cmake");
    cmd.arg("--build").arg(&path_build);
    if matches!(*MODE, Mode::Prod | Mode::Dev) {
        cmd.stdout(Stdio::null());
        cmd.stderr(Stdio::null());
    }

    let status = cmd
        .status()
        .unwrap_or_else(|e| panic!("unexpected error in command invocation: {}", e));
    if !status.success() {
        panic!("build failed, check output for details");
    }

    // execute
    let mut cmd = Command::new(path_build.join(MAIN_EXECUTABLE));
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
    let mut child = cmd.group_spawn().expect("spawning for execution");

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

    // finally, return the output
    Ok(response)
}
