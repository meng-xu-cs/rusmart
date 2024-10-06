## Project Setup Guide

This document provides an overview of the development components, including the Makefile, License, Rust toolchain configuration, gitmodules, gitignore, code coverage setup, Cargo.toml, and Cargo.lock files. These components help streamline the development and testing processes.

### Makefile

The `Makefile` provides a set of predefined commands that help manage the project, including code formatting, dependency management, and documentation generation. In a Makefile, the `@` symbol before a command suppresses the default behavior of make to `echo` the command to the terminal before executing it. By using `@`, the command is executed silently (without printing the command itself), and only the output of the command is shown. The backslashes at the end of each line allow the command to span multiple lines for readability. The `&&` operator is used to run multiple commands sequentially. Below is a description of each command available in the Makefile:

- **help**: Displays a list of available commands and functionalities. Run this command with `make help` to see the options and descriptions.

- **lint**: Formats the code according to Rust's style guidelines using `cargo fmt`, and runs Clippy using `cargo clippy`, a linting tool, to catch common mistakes. Run this command with `make lint` to automatically format and lint the code.

- **cloc**: Counts the total number of lines of code in the specified directories. This command uses the `cloc` tool and can be run with `make cloc`. To install the `cloc` tool on mac, run `brew install cloc`. After installation, you can verify that cloc is successfully installed by running: `cloc --version`.

- **reset**: Resets the project by running `cargo run reset` in the `cli` directory. This directory contains the command line interface of the rusmart tool. Use `make reset` to run this command.

- **deps**: Forces rebuilding the project dependencies (`Z3 and CVC5`). This command runs the `cargo run deps z3 build --force` and `cargo run deps cvc5 build --force` in the `cli` directory and can be invoked with `make deps`.

- **docs**: Cleans, builds, and serves the project documentation using `mdbook`. The documentation will be accessible locally after running `make docs`. Before running this command, ensure that `mdbook` is installed. To install `mdbook`, run `cargo install mdbook`.

- **rego**: Runs the `Rego` language semantics, located in the `lang/rego` directory. This command can be executed with `make rego`.

- **.PHONY**: The `.PHONY: help lint cloc reset deps docs rego` line tells `make` that the listed targets are not files and should always be executed. This ensures that the commands are run even if a file with the same name exists. In other words, Phony targets are not tied to actual files. If a target like `clean` or `help` was not marked as .PHONY and you had a file named `clean` or `help`, make could mistakenly consider it up-to-date and skip executing it. By declaring them .PHONY, make knows that these targets don't correspond to actual files and should be executed every time.

Each of these commands helps automate repetitive tasks.

### License

This project is licensed under the **GNU General Public License (GPL) Version 3**. The GPLv3 is a free, copyleft license that ensures the software remains free and open for all users. Key points include:

- **Freedom to Use, Modify, and Share**: You are free to use, modify, and distribute the software, as long as any derivative work is also licensed under the GPLv3 (copyleft). This ensures that the software remains free and open.
- **Source Code Availability**: If you distribute the software, you must also provide the source code, ensuring others can study and modify it.
- **No Warranty**: The developers are not liable for any issues arising from its use.

For more details, please refer to the `LICENSE` file included in this repository.

### Rust Toolchain

The project uses a `rust-toolchain` file to define the specific toolchain configuration. This file ensures that all developers working on the project are using the same version of Rust, promoting consistency. This file contains the following configuration:

```
[toolchain]
channel = "stable"
components = ["rustfmt", "clippy"]
```
- **Channel**: `stable` – This ensures that the project uses the latest stable version of the Rust compiler. To check the configuration of the Rust toolchain in the root directory you can run `rustup show` to verify the correct channel is active.

- **Components**: 
  - `rustfmt`: A tool that automatically formats Rust code according to standard style guidelines. This improves code readability and reduces the likelihood of stylistic inconsistencies in the sourcecode. Running `cargo fmt` will automatically format the source code. 
  - `clippy`: A linting tool that catches common bugs, and suggests improvements, helping to maintain robustness in the source code. Running `cargo clippy` will lint the code and identify potential issues.

These processes have been automated by the `make lint` command in the Makefile.

### Git Modules

The `.gitmodules` file is used to manage Git submodules. A submodule is essentially a repository within another repository. The content of the file is as following:

```
[submodule "deps/cvc5"]
	path = deps/cvc5
	url = https://github.com/cvc5/cvc5.git
[submodule "deps/z3"]
	path = deps/z3
	url = https://github.com/Z3Prover/z3.git
```

`path = deps/cvc5` Specifies where the submodule will be located in the project's directory structure. In this case, the cvc5 submodule will be located in the `deps/cvc5` directory.
url = https://github.com/cvc5/cvc5.git: Specifies the URL of the repository for the submodule. This is the repository that will be cloned into the `deps/cvc5` directory - same reasoning stands for the z3 submodule. When cloning the main repository, the submodules are not automatically cloned by default. After cloning, one can run `git submodule init` and `git submodule update` to clone the submodules into their respective paths. Submodules are independent repositories, so they have their own commit histories and can be updated independently of the main repository.

### Gitignore

The `.gitignore` file specifies files and directories that should be ignored by Git. This helps prevent unnecessary files from being included in the repository when being pushed. The `.gitignore` file in this project includes the following entries:

```
# artifacts
/studio/
/target/

# IDE
/.idea/

# MacOS
.DS_Store

# Z3 trace files
.z3-trace

# todo list
todo.txt
```

### Code Coverage 

`Coverage gutters` is an extension for Visual Studio Code that visualizes code coverage information in the editor. It searches for `lcov.info` files in the project root directory and visualizes the code coverage information inline. To generate the `lcov.info` file, you can use tools like `grcov` or `cargo tarpaulin`. The steps are as follows:

1. Install the `Coverage Gutters` extension in Visual Studio Code.
2. Install the `cargo-tarpaulin` crate by running `cargo install cargo-tarpaulin`.
3. Run `cargo tarpaulin --out Lcov` to generate the `lcov.info` file.

Depending on where we run the command `cargo tarpaulin --out Lcov`, the content of the `lcov.info` file will be different. If we have a workspace with multiple crates for example, the `lcov.info` file will always be created in the root of the workspace. However, if the command is run in the root of the workspace, the lcov.info file will contain coverage information for all the crates in the workspace. If the command is run in the directory of a specific crate, the lcov.info file will contain coverage information only for that specific crate. Now if we run the command `cargo tarpaulin --out Lcov --out html`, the coverage information will be generated in both lcov.info and html format. The html file will be named as `tarpaulin-report.html` by default. Note that the html file will always be created in the root of the workspace as well, unless specified by the `--output-dir` flag otherwise. To reiterate, depending on where the command is run, the content of the html file will be specific to that crate if the command is run in the directory of a specific crate; or the entire workspace if the command is run in the root of the workspace. In either way, all the crates will be listed in the html file, but the coverage information will be different. We can also only generate the html file by running `cargo tarpaulin --out html`.

Now by openning the command pallet (cmd + shift + p in mac) and typing coverage, we can see the commands available from the `coverage gutters` extension. We have two important commands `watch/unwatch coverage`. The watch coverage will show which functions are covered and which are not by the test suite. The covered functions will have a green mark and the uncovered functions will have a red mark (you can customize the viewing in the extension settings. For example adding a show ruler/line/gutter option). Note that as we write new tests (or delete the previous ones), the inline display of the coverage in the editory is not automatically updated. To update the coverage information, we need to run the tarpaulin command again and then toggle the watch coverage command to see the new coverage information. The default version of cargo tarpaulin is `--ignore-tests`, which means that the coverage information will not be generated for the tests.

If we want to automatically update the coverage display inline in the editor whenever we write new tests, instead of running the tarpaulin command again and then toggling the watch coverage, we can do the following:

1. Install the `cargo-watch` tool by running `cargo install cargo-watch`.
2. Run the command `cargo watch -x 'tarpaulin --out Lcov' -i lcov.info` to automatically run the tarpaulin command whenever a file in the project changes. Note that do not write `cargo tarpaulin` here as the cargo is implicit.

`cargo-watch` is a command-line utility for Rust developers that automatically monitors changes in your project's files and executes specified commands whenever a change is detected. The `-x` flag tells cargo watch to execute what command whenever a change is detected. We can use this tool to run the tarpaulin command whenever a file in the project changes. This way, a new `Lcov` file will be automatically created whenever a source code is updated, and subsequently the inline coverage (provided by coverage gutters) will be updated as well. Note that coverage gutters provides the inline coverage in the editor by looking at the `Lcov` file. The `-i lcov.info` flag is used to ignore the lcov.info file when it changes. This is because the tarpaulin command will generate the lcov.info (in the root of the workspace by default) when it runs, so if we don't ignore it, the cargo watch command will run the tarpaulin command again when the lcov.info file changes, which will consequently create an infinite loop. So we need to ignore the lcov.info file when it changes. However, we can remove the -i lcov.info flag if the directory that we are watching and the directory where the lcov.info file is generated are different. For example, if we want to run the tarpaulin command inside a specific crate directory, because the lcov.info file will be created in the root of the workspace, it will not be in the directory of the crate, thus automatically ignoring it. To emphasize cargo watch only watches the files in the directory where it is run. However, the cargo tarpaulin will generate files in the root of the workspace by default. Lastly, instead of saying what to ignore, we can say what to include. For example, we can say `-w src/` to `watch` only the src directory. so the command will be `cargo watch -x 'tarpaulin --out Lcov' -w src`. The path must exist in the project. Note that the `-w` flag is only for the cargo watch command. The tarpaulin command will run for the entire workspace unless we specify otherwise. To have the lcov.info file generated in the target directory, we can use the `--output-dir` flag. For example, `cargo tarpaulin --out Lcov --output-dir target/tarpaulin`. This will generate the lcov.info file in the target/tarpaulin directory. Note that this directory already exists. This way we will have the file abstracted from the root of the workspace. Also whenever we run `cargo clean`, the target directory will be deleted, so the lcov.info file will be deleted as well.

To simplify the process we have written a shell script that automates the process of generating and viewing code coverage reports. There is one `cov.sh` file in the root of the workspace and one in each crate directory. The content of the `cov.sh` file in the root of the workspace is as follows:

```bash
#!/bin/bash

#cleaning up the previous coverage files
cargo clean

cargo tarpaulin \
    --engine llvm \
    --out Lcov \
    --out html \
    --output-dir target/tarpaulin \

open target/tarpaulin/tarpaulin-report.html
```

The first line is the shebang line, which tells the system which interpreter should be used to execute the script. The script uses [Tarpaulin](https://github.com/xd009642/tarpaulin), a popular code coverage tool for Rust projects. It runs cargo tarpaulin with the `--engine llvm` flag specifying that tarpaulin should use the `LLVM-based coverage` engine. The LLVM engine tends to be more accurate and can handle more complex code constructs than the default engine. The `--out html` flag specifies the output format of the coverage report. Finally, the script creates the `Lcov` and `html` files in the `target/tarpaulin` directory. The html file by default is named `tarpaulin-report.html`, which in the last line is opened. You can run the script by first installing Tarpaulin - if you haven't already - by running the following command: `cargo install cargo-tarpaulin`. Then, make the script executable by running `chmod +x cov.sh`, and finally run it by executing `./cov.sh`. While running the script, `.profraw` files are generated in the `target/tarpaulin/profraws` directory - which is automatically created. These files are created by the LLVM engine to store raw profiling data collected by the tarpaulin to calculate coverage statistics.


As you can see each crate has a separate `cov.sh` file as well, which the content of each is the same and includes the following:

```bash
#!/bin/bash

cargo clean

cargo watch -x \
      'tarpaulin --engine llvm --out Lcov --output-dir target/tarpaulin' \
      -w src/
```

After cleaning the project (erasing any `Lcov` file as it was located in the `target` directory), the script runs the `cargo watch` command. The command `tarpaulin --engine llvm --out Lcov --output-dir target/tarpaulin` is only continuously run whenever a file inside the `src/` directory of the specific crate (where the cov.sh exists) is changed. The tarpaulin command given that it is run from a crate, only generates coverage statistics for that crate in the Lcov.info file. Therefore, when we toggle the coverage watch command provided by the coverage gutter extension, we can `on-the-fly` see the inline coverage information for the source code of that specific crate. Writing each `cov.sh` file for each crate is especially useful because the tarpaulin command can take a long time to run in large projects. So to create new unit tests and track the coverage, we can run the tarpaulin command only for the specific crate where we are working. To make all the `cov.sh` files executable, run the following command from the root of the workspace `find . -name "cov.sh" -exec chmod +x {} \;`.

Last but not least, the `coverage.sh` file in the root of the workspace, utilizes the `grcov` tool to generate a code coverage report for the entire workspace. This is different than the `tarpaulin` tool. We will not go into the details of the differences between the two tools. We have used `tarpaulin` for our project to track the coverage of the unit tests. Note that having multiple `lcov.info` files in the workspace will affect the coverage information displayed by the coverage gutters extension. That is why we place the `lcov.info` file in the `target/tarpaulin` directory and we clean the project at the beginning of each script.

The rusmart project is a `Rust Workspace`, which essentially is a set of packages (crates) that share the same `Cargo.lock` and `output directory`. The `Cargo.toml` file in the root directory configures the entire workspace. This file will start with a `workspace` section that will allow us to add members to the workspace by specifying the path to the packages. The workspace has one `target` directory at the top level that the compiled artifacts will be placed into by running `cargo build`. Even if we were to run `cargo build` from inside an inner crate directory, the compiled artifacts would still end up in the root `target` directory rather than in the `target` directory of the crate. If each crate had its own target directory, each crate would have to recompile each of the other crates in the workspace to place the artifacts in its own target directory. By sharing one target directory, the crates can share the compiled artifacts and avoid recompiling. Notice that the workspace has only one `Cargo.lock` file at the top level, rather than having a Cargo.lock in each crate’s directory. This ensures that all crates are using the same version (`Semantic Versions (SemVars)`) of all dependencies. Note that because the workspace contains multiple binary crates, we cannot run the `cargo run` command directly. Instead, we must specify the binary crate we want to run by using the `--bin` flag. For example, to run the `rusmart-cli` crate, we would run the `cargo run --bin rusmart-cli` command. Lets take a look at the Cargo.toml file in the root directory of the workspace:

### Cargo.toml

The workspace `Tom's Obvious Minimum Language` (TOML) file contains the following content:

```
[workspace]
members = [
    # ordered and grouped loosely by dependency relation
    "utils",
    "cli",
    "smt/stdlib",
    "smt/remark",
    # if "smt/remark/remark_derive" was not listed as a member and was only listed as a dependency:
    # 1) It will not share the workspace's Cargo.lock file.
    # 2) It will not share the target directory.
    # 3) It will be built in isolation from the workspace members.
    "smt/remark/remark_derive",
    "smt/derive",
    "smt/testing",
    "lang/demo",
    "lang/rego",
]
resolver = "2"

[workspace.dependencies]
anyhow = "1.0.86"
clap = { version = "4.5.13", features = ["derive"] }
command-group = "5.0.1"
datatest-stable = "0.2.9"
internment = { version = "0.8.4", features = ["arc"] }
itertools = "0.13.0"
lazy_static = "1.5.0"
log = "0.4.22"
num_cpus = "1.16.0"
num-bigint = "0.4.6"
num-rational = "0.4.2"
num-traits = "0.2.19"
paste = "1.0.15"
petgraph = "0.6.5"
proc-macro2 = "1.0.86"
quote = "1.0.36"
syn = { version = "2.0.72", features = ["full", "extra-traits"] }
tempfile = "3.11.0"
simplelog = "0.12.2"
walkdir = "2.5.0"
trybuild = "1.0"


rusmart-utils = { path = "utils" }
rusmart-cli = { path = "cli" }
rusmart-smt-stdlib = { path = "smt/stdlib" }
rusmart-smt-remark = { path = "smt/remark" }
rusmart-smt-remark-derive = { path = "smt/remark/remark_derive" }
rusmart-smt-derive = { path = "smt/derive" }
```

This file indicates that the project is organized as a Rust workspace, comprising multiple packages (rusmart-utils, rusmart-cli, rusmart-smt-stdlib, rusmart-smt-remark, rusmart-smt-remark-derive, rusmart-smt-derive, rusmart-smt-testing, rusmart-lang-demo, rusmart-lang-rego) that are grouped and loosely ordered by their dependency relationships. In short, `loosely ordered` means that the packages downstream in the dependency graph are listed after the packages they depend on, though this is not restricted. Furthermore, the workspace relies on several external crates, which are managed collectively under [workspace.dependencies]. All the crates in the workspace share the same version of these dependencies. The dependecies in short provide the following functionality:

- **proc-macro2**, **quote**, **syn**: Crates essential for procedural macro development. The `syn` crate parses Rust code from a string into a data structure that we can perform operations on. The `quote` crate turns syn data structures back into Rust code. These crates make it much simpler to parse any sort of Rust code we might want to handle.
- **anyhow**: Provides flexible error handling.
- **clap**: A command-line argument parser.
- **command-group**: A library for grouping commands in a CLI.
- **datatest-stable**: A testing framework for data-driven tests.
- **internment**: Provides the `Intern` type for interning values which is useful for reducing memory usage given that the same value is stored only once. Also it allows for wrapping the values.
- **itertools**: Extends iterators with additional methods. The macro `iproduct!` is useful for creating a cartesian product of iterators.
- **lazy_static**: Enables the creation of statically initialized, lazily evaluated variables.
- **log**: A logging facade for Rust.
- **num-cpus**: Retrieves the number of CPUs available.
- **num-bigint**, **num-rational**, **num-traits**: Provide utilities for working with numbers, including big integers, rational numbers, and numeric traits.
- **paste**: Concatenates identifiers during macro expansion.
- **petgraph**: Graph data structure library.
- **tempfile**: Generates temporary files.
- **simplelog**: Simplified logging framework.
- **walkdir**: Efficient directory traversal.

The workspace also includes internal crates and their respective paths.

### Cargo.lock

As mentioned earlier, the workspace has only one `Cargo.lock` file at the top level, rather than having a `Cargo.lock` in each crate’s directory. This ensures that all crates are using the same version of all dependencies. The `Cargo.lock` file is a file that Cargo generates to keep track of the exact versions of dependencies that are used in the project. The `Cargo.lock` file is automatically generated by Cargo when we run `cargo build` and is not meant to be edited manually.

### Summary

In short, the Makefile automates common tasks, the GPLv3 license ensures the software remains free and open, and the Rust toolchain configuration guarantees that all developers are using a consistent setup. The gitmodules and gitignore files manage the repository structure, and the code coverage setup helps track test coverage. The Cargo.toml and Cargo.lock files define the project's dependencies and configuration. Together, these components streamline the development and testing processes.
