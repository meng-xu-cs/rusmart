use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use lazy_static::lazy_static;

use rusmart_cli::cli::expect_cvc5;
use rusmart_utils::config::{Mode, MODE};

use crate::backend::codegen::CodeGen;
use crate::backend::error::BackendResult;
use crate::backend::exec::{mk_shell_script, run_backend, Response};
use crate::ir::ctxt::IRContext;

/// CVC5 artifact wrapper
struct ArtifactCVC5 {
    path_include: PathBuf,
    path_library: PathBuf,
}

impl ArtifactCVC5 {
    pub fn new() -> Self {
        let base = expect_cvc5();
        Self {
            path_include: base.join("include"),
            path_library: base.join("lib"),
        }
    }
}

lazy_static! {
    static ref ARTIFACT_CVC5: ArtifactCVC5 = ArtifactCVC5::new();
}

/// A generic backend for CVC5-related
pub trait BackendCVC5 {
    /// Mark the name of the backend
    fn name(&self) -> String;

    /// Produce the backend-specific code for the SMT model
    fn process(&self, ir: &IRContext) -> BackendResult<String>;
}

/// Wrapper for the backend
pub struct CodeGenCVC5<T: BackendCVC5> {
    backend: T,
}

impl<T: BackendCVC5> CodeGenCVC5<T> {
    pub fn new(backend: T) -> Self {
        Self { backend }
    }
}

impl<T: BackendCVC5> CodeGen for CodeGenCVC5<T> {
    fn name(&self) -> String {
        self.backend.name()
    }

    fn execute(&self, ir: &IRContext, path_wks: &Path) -> BackendResult<Response> {
        // generate
        let code = self.backend.process(ir)?;

        let path_src = path_wks.join("main.cpp");
        if path_src.exists() {
            panic!("main.cpp already exists");
        }
        fs::write(&path_src, code).unwrap_or_else(|e| panic!("IO error on source code: {}", e));

        // prepare the compile command
        let path_bin = path_wks.join("main");
        let mut command = Command::new("c++");
        command
            .arg("-std=c++20")
            .arg("-I")
            .arg(&ARTIFACT_CVC5.path_include)
            .arg("-L")
            .arg(&ARTIFACT_CVC5.path_library)
            .arg("-lcvc5")
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
        command.env("DYLD_LIBRARY_PATH", &ARTIFACT_CVC5.path_library);

        // produce a script for local debugging
        if matches!(*MODE, Mode::Debug | Mode::Verbose) {
            mk_shell_script(&command, &path_wks.join("solve.sh"));
        }

        // hand it for execution
        let response = run_backend(command);

        // done
        Ok(response)
    }
}
