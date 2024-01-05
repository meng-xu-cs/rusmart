use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use lazy_static::lazy_static;

use rusmart_cli::cli::expect_z3;
use rusmart_utils::config::{Mode, MODE};

use crate::backend::codegen::CodeGen;
use crate::backend::error::BackendResult;
use crate::backend::exec::{mk_shell_script, run_backend, Response};
use crate::ir::ctxt::IRContext;

/// Z3 artifact wrapper
struct ArtifactZ3 {
    path_include: PathBuf,
    path_library: PathBuf,
}

impl ArtifactZ3 {
    pub fn new() -> Self {
        let base = expect_z3();
        Self {
            path_include: base.join("include"),
            path_library: base.join("lib"),
        }
    }
}

lazy_static! {
    static ref ARTIFACT_Z3: ArtifactZ3 = ArtifactZ3::new();
}

/// A generic backend for Z3-related
pub trait BackendZ3 {
    /// Mark the name of the backend
    fn name(&self) -> String;

    /// Produce the backend-specific code for the SMT model
    fn process(&self, ir: &IRContext) -> BackendResult<String>;
}

/// Wrapper for the backend
pub struct CodeGenZ3<T: BackendZ3> {
    backend: T,
}

impl<T: BackendZ3> CodeGenZ3<T> {
    pub fn new(backend: T) -> Self {
        Self { backend }
    }
}

impl<T: BackendZ3> CodeGen for CodeGenZ3<T> {
    fn name(&self) -> String {
        self.backend.name()
    }

    fn execute(&self, ir: &IRContext, path_wks: &Path) -> BackendResult<Response> {
        // generate
        let code = self.backend.process(ir)?;

        let path_src = path_wks.join("main.c");
        if path_src.exists() {
            panic!("main.c already exists");
        }
        fs::write(&path_src, code)
            .unwrap_or_else(|e| panic!("IO error on writing to main.c: {}", e));

        // prepare the compile command
        let path_bin = path_wks.join("main");
        let mut command = Command::new("cc");
        command
            .arg("-I")
            .arg(&ARTIFACT_Z3.path_include)
            .arg("-L")
            .arg(&ARTIFACT_Z3.path_library)
            .arg("-lz3")
            .arg("-o")
            .arg(&path_bin)
            .arg(&path_src);

        // produce a script for local debugging
        if matches!(*MODE, Mode::Debug | Mode::Verbose) {
            mk_shell_script(&command, &path_wks.join("build.sh"));
        }

        // run the compile command
        let status = command
            .status()
            .unwrap_or_else(|e| panic!("unexpected error in command invocation: {}", e));
        if !status.success() {
            panic!("compilation failed, check output for details");
        }

        // prepare the execution command
        let mut command = Command::new(&path_bin);
        #[cfg(target_os = "macos")]
        command.env("DYLD_LIBRARY_PATH", &ARTIFACT_Z3.path_library);

        // produce a script for local debugging
        if matches!(*MODE, Mode::Debug | Mode::Verbose) {
            mk_shell_script(&command, &path_wks.join("solve.sh"));
        }

        // hand it for execution
        let response = run_backend(command);

        // occasionally, z3 leaves a trace file, remove it
        let log_z3_trace = Path::new(".z3-trace");
        if log_z3_trace.exists() {
            fs::remove_file(log_z3_trace).expect("removing .z3-trace");
        }

        // done
        Ok(response)
    }
}
