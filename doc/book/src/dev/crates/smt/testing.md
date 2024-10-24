### Testing
---

#### Cargo.toml

The package located in the _rusmart-smt-testing_ directory contains the __integration tests__ for the _rusmart-smt-derive_ package. The _Cargo.toml_ file for the package is shown below:

```toml
[package]
name = "rusmart-smt-testing"
description = "Test suites for the SMT component"
version = "0.1.0"
edition = "2021"
authors = ["Meng Xu <meng.xu.cs@uwaterloo.ca>"]
license = "GPL-3.0"

[dev-dependencies]
anyhow = { workspace = true }
datatest-stable = { workspace = true }
tempfile = { workspace = true }
rusmart-utils = { workspace = true }
rusmart-smt-stdlib = { workspace = true }
rusmart-smt-remark-derive = { workspace = true }
rusmart-smt-derive = { workspace = true }

[lints.rust]
non_camel_case_types = "allow"
unused = "allow"

[lints.clippy]
all = "allow"

[[test]]
name = "integration"
harness = false
```

This file is used to define the metadata, where the _package_ section is similar to the _package_ section of the _Cargo.toml_ files of other packages in the workspace. The name, description, version, edition, authors, and license fields are self-explanatory. The _dev-dependencies_ section is different than the _dependencies_ section seen in the _Cargo.toml_ files of other packages. Accordingly, a _dev-dependencies_ section has a similar format to the _dependencies_ section. However, _dev-dependencies_ are not used when compiling a package for building, but are used for compiling tests, examples, and benchmarks. Moreover, these dependencies are not propagated to other packages which depend on this package. In simple words, these dependencies will not be included when executing `cargo build`. See the [Cargo documentation](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#dev-dependencies) for more information. 

The _anyhow_ library provides _anyhow::Error_, a trait object based error type for easy idiomatic error handling in Rust applications. It also provides the Result<T> type, which is an alias for Result<T, anyhow::Error>. The _anyhow_ macro is used to create an error from a string or an error type that can be converted into an _anyhow::Error_. The _bail_ macro is used to return an error from a function without explicitly mentioning return and is equivalent to _return Err(anyhow!(...))_. The _Context_ trait allows you to add extra context when an operation returns an error. This is done by calling the _context_ or _with_context_ methods on a Result. For more information, see the [anyhow documentation](https://docs.rs/anyhow/1.0.40/anyhow/).

The name in the _\[\[test\]\]_ section suggests that this package is devoted to _integration testing_. Also, the value assigned to name refers to the file containing the test code. Cargo will look for a source file with the given name in the _tests/_ directory (i.e., in this case _tests/integration.rs_). Furthermore, the libtest harness needs to be disabled in the _\[\[test\]\]_ section by setting _harness = false_, in which case the code will need to provide its own _main_ function to handle running tests. The _datatest_stable::harness!_ macro internally generates a main() function that orchestrates the test execution, thus no manual main() function is necessary. To access the harness macro, the _datatest-stable_ library is added as a dev-dependency. Accordingly, the _datatest-stable_ library provides a procedural macro - __harness__ - for writing _file-driven_ or _data-driven_ tests, where individual test cases are specified as files and not as code. The __harness__ macro given a function _my\_test_ that accepts a path, a directory to look for files in, and a pattern to match files on, will call the _my\_test_ function once per matching file in the directory. All in all, the harness macro is used inside the _integration.rs_ file to run the integration tests defined in separate files. In this case, because the default libtest harness is disabled, __#\[test\]__ attributes are not required in the test files.

The _tempfile_ library provides a secure and easy way to create temporary files and directories. The _tempfile::tempdir_ function creates a temporary directory and returns a _TempDir_ object. The _TempDir_ object is a handle to a temporary directory that deletes the directory when it goes out of scope. 

_\[lints.rust\]_ and _\[lints.clippy\]_ are used to suppress warnings from the Rust and Clippy linters, respectively. The former configures Rust's built-in lints (the ones that __rustc__ enforces) and the latter configures the behavior of __clippy__, which is an additional linting tool for Rust that provides more advanced and stylistic lints than the default Rust lints. The _non_camel_case_types_ and _unused_ lints are set to _allow_ in the _\[lints.rust\]_ section, while all lints are set to _allow_ in the _\[lints.clippy\]_ section. The _non_camel_case_types_ lint is used to suppress warnings about types that do not follow the camel case convention. The _unused_ lint is used to suppress warnings about unused code. The _all_ lint is used to suppress all warnings from the Clippy linter.
---
#### Test Organization

Before delving deep into the source code, let's first talk about test organization in rust. In Rust, integration tests are entirely external to your library. They use your library in the same way any other code would, which means they can only call functions that are part of your library’s _public API_. To organize integration tests within a package, we create a _tests_ directory at the top level of our project directory, next to _src_. Cargo knows to look for integration test files in this directory. Inside _tests_, we can create any number of test files, and Cargo will compile each of the files as an individual crate. Nevertheless, in the case of this project, which is a workspace, as the integration tests involve multiple packages - _rusmart-smt-derive_, _rusmart-smt-remark-derive_, _rusmart-smt-stdlib_, and _rusmart-utils_ - a separate package has been created for the integration tests. If our package was a binary crate that only contained a src/main.rs file and didn't have a src/lib.rs file, we couldn't create integration tests in the tests directory. Therefore, the _testing_ package consists of a library crate with a _lib.rs_ file and a _tests_ directory containing the integration tests. The _lib.rs_ file in this case is empty as the tests target the API of external crates. The _tests_ directory contains the _integration.rs_ file, which is the entry point for the integration tests. In the normal configuration, the _integration.rs_ file would be a  test file and will appear in the test output. However, as we have disabled the libtest harness, the _integration.rs_ file will not appear in the test output. Instead, the harness macro in the _integration.rs_ file will run the integration tests defined in separate files located in the _testing/tests/model_ directory. This is because the _integration.rs_ file contains the following code:

```rust
harness!(test_model, "tests/model", r"^.*\.rs$");
```

As you can see, the _harness!_ macro is used to run the _test\_model_ function for each file in the _tests/model_ directory that matches the regular expression _^.*\.rs$_. The regular expression matches all Rust files with the _.rs_ extension. The explanation of the regex is as follows:

- _^_ asserts the start of a line.
- _.*_ matches any character (except for line terminators) between zero and unlimited times, as many times as possible, giving back as needed (greedy). The dot matches any character except for line terminators. Line terminator examples are \n, \r, \r\n, etc.
- _\.rs_ matches the characters _.rs_ literally (case sensitive). The backslash is used to escape the dot character.
- _$_ asserts the end of a line.

Note that the directory given to the harness macro is relative to the _Cargo.toml_ file of the package. Also, the harness macro is defined recursively, so that the function is applied to all files matching the pattern in the directory and its _subdirectories_.
---

#### The source code of integration.rs

The code defines two main functions: 

1) _check_mod_, which checks the consistency between the _mod.rs_ file and the directory contents
2) _test_model_, which is a generic test runner for all front-end test cases. 

The _check_mod_ function reads the _mod.rs_ file and collects all module names declared in it. It then reads the directory containing the respective _mod.rs_ file and collects all Rust file names, either as subdirectories containing a _mod.rs_ file or as _.rs_ files. The function then ensures that there is a one-to-one correspondence between the module names declared in the _mod.rs_ file and the file/directory names in the directory. If there is a mismatch, an error is returned.

If you see the top level _mod.rs_ file in the _testing/tests/model_ directory, it contains the following code:

```rust
mod analysis;
mod backend;
mod ir;
mod parser;
```

This means that there are four directories in this case, present in the _testing/tests/model_ directory, with the same names as the modules declared in the _mod.rs_ file. Each of these directories will contain a _mod.rs_ file listing the test files for the respective module. The test files under each of these directories target the corresponding modules in the _rusmart-smt-derive_ package.

The _test_model_ function handles test files and compares their output to expected error results (if present). The function will return an error in the following cases:

- the `mod.rs` is not as expected according to the check_mod function.
- There is an issue with reading or writing test files.
- the test file passes but does not have an `_ok.rs` suffix. This means that all passing tests should have an `_ok.rs` suffix.
- the test file passes but an expected error output file exists while not updating the baseline. This means that a passing test should not have an expected error output file.
- the test file fails but has an `_ok.rs` suffix. This means only passing tests should have an `_ok.rs` suffix.
- the test file fails but does not have an expected error output file while not updating the baseline. This means that failing tests must have an expected error output file.
- the test file fails but the expected error output file does not match the actual output while not updating the baseline.

Updating the baseline can happen by executing the command `UPBL=1 cargo test` in the terminal. This will update the _.exp_ files. 
---

#### Test Files

In summary, the tree structure of the _testing/tests/model_ directory is as follows:

- _testing/tests/model_
  - _mod.rs_ (contains module declarations)
  - _analysis_
    - _mod.rs_ (contains test files for the analysis module)
    - _test1_ok.rs_ (passing test)
    - _test2_ok.rs_ (passing test)
    - _test3.rs_ (failing test)
    - _test3.exp_ (expected error output for failing test)
  - _backend_
    - _mod.rs_ (contains test files for the backend module)
    - _test1_ok.rs_ (passing test)
    - _test2_ok.rs_ (passing test)
    - _test3.rs_ (failing test)
    - _test3.exp_ (expected error output for failing test)
  - _ir_
    - _mod.rs_ (contains test files for the ir module)
    - _test1_ok.rs_ (passing test)
    - _test2_ok.rs_ (passing test)
    - _test3.rs_ (failing test)
    - _test3.exp_ (expected error output for failing test)
  - _parser_
    - _mod.rs_ (contains test files for the parser module)
    - _test1_ok.rs_ (passing test)
    - _test2_ok.rs_ (passing test)
    - _test3.rs_ (failing test)
    - _test3.exp_ (expected error output for failing test)
  
Also, the functions defined in the _rustmart-smt-testing_ package are not included in the code coverage report generated by the _tarpaulin_ tool. This is because these functions are _integration tests_ for other functions in the _rusmart-smt-derive_ package, and thus, are not part of the code logic. That is why the line __--exclude-files "smt/testing/*.rs" __ is added to the _tarpaulin_ command in _cov.sh_ in the root directory of the workspace. This command is used to exclude the files in the _smt/testing_ directory from the code coverage report. Having said all that, the two functions _test\_model_ and _check\_mod_ in _integration.rs_ are not integration tests, but rather test utilities for organizing and running integration tests. They contain minimal logic and thus are not included in the code coverage report as well.


