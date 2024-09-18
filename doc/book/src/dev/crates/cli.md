### Preliminary Information

_Z3_ and _CVC5_ are mentioned here because they play a key role in how the _rusmart-cli_ package operates. Before describing the crate and the modules the package contains, I will first want to reflect on the _.gitmodules_ file, which has the following content:

```bash
[submodule "cvc5 dependency"]
	path = deps/cvc5
	url = https://github.com/cvc5/cvc5.git
[submodule "z3 dependency"]
	path = deps/z3
	url = https://github.com/Z3Prover/z3.git
```

The first lines _[submodule "cvc5 dependency"]_ and _[submodule "z3 dependency"]_ are the names of the submodules. When we run the command `git submodule init` the following is printed as the _stdout_:

```bash
Submodule 'cvc5 dependency' (https://github.com/cvc5/cvc5.git) registered for path 'deps/cvc5'
Submodule 'z3 dependency' (https://github.com/Z3Prover/z3.git) registered for path 'deps/z3'
```

Submodules in Git are essentially repositories within a repository. They're useful when you want to include external dependencies or other projects within your project, but you want to keep them separate for version control purposes. The `git submodule init` command initializes the submodules by reading the .gitmodules file and setting up local configuration for each submodule. It doesn’t clone the submodule repositories yet; it just records that the submodules and their paths exist. By running this command, git knows where each submodule is located and where to get it (using the url field in the .gitmodules file). If the gitmodules file is not changed after the previous run, by running this command again, nothing will be printed in the _stdout_. The `git submodule update` command is used to fetch and check out the submodules. Git will clone the repositories for each submodule (if they haven't been cloned already) and check out the commit. Essentially, it brings the submodule repositories (deps/cvc5 and deps/z3) to the correct state/commit.

---

### Commands

The _rusmart-cli_ package provides additional functionality to the `cargo run` CLI. Specifically, in the `Makefile` in the root directory of the workspace, the following commands are relevant to this discussion:

```bash
reset:
	@cd cli && cargo run reset

deps:
	@cd cli && \
		cargo run deps z3 build --force
	@cd cli && \
		cargo run deps cvc5 build --force
```

The commands `make reset` and `make deps`, run the two mentioned commands. We will now describe the two commands in more detail.

1. `@cd cli && cargo run reset`: This command includes a _@_ indicating that when `make reset` is executed only the output of the command will be printed to the _stdout_, and not the command itself (the command will not be echoed). The command changes the directory to the _cli_ directory and runs the command `cargo run reset`. `cargo run` has been given a parameter `reset` which is not a default parameter. When running the package, the _main_ function of the _binary crate_ will be invoked. This function initializes the workspace and the configurations. Then it parses the command line using the _clap_ crate. If the parameter is _reset_, the _studio/native (or alternatively studio/docker)_ directory will be removed. If the directory does not exist, _The studio directory does not exist; there is nothing to reset..._ will be printed to _stderr_.

2. `@cd cli && \ cargo run deps z3 build --force`: This command includes a _@_ indicating that when `make deps` is executed only the output of the command will be printed to the _stdout_, and not the command itself. The command changes the directory to the _cli_ directory and executes the command `cargo run deps z3 build --force`. `cargo run` has been given a parameter `deps z3 build --force` which is not a default parameter. When running the package, the _main_ function of the _binary crate_ will be invoked. This function initializes the workspace and the configurations. Then it parses the command line using the _clap_ crate. If the subcommand is _deps_, then the subcommand (sub-subcommand in this case 🙃) `z3 build --force` will be executed. Depending on whether the subcommand is `z3 build --force` or `cvc5 build --force`, the coressponding action will be taken internally.
	- For _Z3_, first the programs checks whether the _studio/native/deps/Z3/<commit hash> (or alternatively _studio/docker/deps/Z3/<commit hash> for docker containers)_ directory is an _artifact_ or not. An _artifact_, is a directory that exists and contains the _src_ and _dst_ subdirectories (each of which contain specific configuration files). If the directories exist, because the rebuilt is forced, a _warning message_ __Force rebuilding package__ will be printed and the base directory will be removed. If the rebuilt was not forced an _informational message_ __Package already exists__ would have been printed and nothing would happen. Nevertheless, if the directory _studio/native/deps/Z3/<commit hash> (or alternatively _studio/docker/deps/Z3/<commit hash> for docker containers)_ does not exist, or the rebuilt is forced, then the _Z3_ dependecy is built. This happens such that the _studio/native/deps/Z3/<commit hash>_ directory (or alternatively _studio/docker/deps/Z3/<commit hash> for docker containers)_ directory is created. Subdirectories _src_ and _dst_ are also created. Furthermore, from the _deps/Z3_ directory the repository is cloned and checked out to the _src_ subdirectory. Therefore, before this stage you need to be sure that the git submodule exists in the _deps/Z3_ directory. To ensure this you should have executed the prementioned `git submodule init` and `git submodule update` commands. Last but not least, the _Z3_ dependency is built. To do so a subdirectory _build_ is created in the <src> _studio/native/deps/Z3/<commit hash>/src (or alternatively _studio/docker/deps/Z3/<commit hash>/src for docker containers)_ path. The _cmake_ command is used to configure the build. Specifically, `cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -DZ3_SINGLE_THREADED=TRUE <studio/native/deps/Z3/<commit hash>/src>` is executed in the _build_ directory. Then the build is executed by running `cmake --build <studio/native/deps/Z3/<commit hash>/src/build>`. The _install_ is executed by running `cmake --install <studio/native/deps/Z3/<commit hash>/src/build> --prefix <studio/native/deps/Z3/<commit hash>/dst>`.

	- For _CVC5_, the same procedure is followed as for _Z3_. The only difference is that the _CVC5_ repository is cloned from the _deps/CVC5_ directory and checked out to the corresponding _src_ subdirectory. Also the build commands are different; the `./configure.sh debug --prefix=<studio/native/deps/CVC5/<commit hash>/dst> --ninja --gpl --auto-download` command is executed in the _src_ directory. The build is executed by running `ninja` in the _build_ directory. The _install_ is executed by running `ninja install` in the _build_ directory.

Note that these are the internal implementations. When running `make deps`, the user does not see the internal implementation. The user only sees the output of the command, for example the messages and the artifacts that are created. To finalize the explanations on the command a brief overview of the function call is provided: 

- main
	- initialize workspace (calling rusmart_utils::config::initialize)
	- parse command line
		- if reset
			- reset workspace
		- if deps
			- run subcommand for depargs (in cli.rs)
		        - run_internal for depaction
					- creating a new depstate (in dep.rs)
						- if action Config
							- configuring the dependency
						- if action Build
							- building the dependency

Other commands can be run as an extension of `cargo run` in the _rusmart-cli_ package. For example, the following command can be run:

```bash
cargo run deps cvc5 build
cargo run deps z3 build
```

The above commands are similar but are not forced. Therefore if the _studio/native/deps/Z3/<commit hash> (or alternatively _studio/docker/deps/Z3/<commit hash> for docker containers)_ for _Z3_ or _studio/native/deps/CVC5/<commit hash> (or alternatively _studio/docker/deps/CVC5/<commit hash> for docker containers)_ for _CVC5_ directories exist, the message __Package already exists__ will be printed and nothing will happen. Only when the directories do not exist, the _Z3_ or _CVC5_ dependency will be built.

Other commands that exist are: `cargo run deps z3 config` and `cargo run deps cvc5 config`. These commands create a temporary _artifact_ directory and clone the git repository into the _src_ subdirectory. Then the configurations of the _Z3_ and _CVC5_ dependencies are listed. The directory is removed after the configurations are listed.

Lastly, the `cargo run -- --help` command can be run. This command shows the help message for the binary and is provided by default by the _clap_ crate. In this case, the following configuration is used:

```rust
#[command(
    name = "semantic-smt-cli",
    about,
    version,
    author,
    rename_all = "kebab-case"
)]
#[command(
    help_template = "Tool: {name}\nAuthor: {author-with-newline}Version: {version}{about-section}\n{usage-heading} {usage} \n {all-args} {tab}"
)]
```

As a result, the help message will be as follows:

```
Tool: semantic-smt-cli
Author: Meng Xu <meng.xu.cs@uwaterloo.ca>
Version: 0.1.0
A command line interface for the Rusmart project
Usage: rusmart-cli <COMMAND>
Commands:
reset  Wipe-clean the entire workspacedksvjdfsjkn
deps   Manage dependencies (subcommand is defined in DepArgs)
help   Print this message or the help of the given subcommand(s)
Options:
-h, --help     Print help
-V, --version  Print version
```

Note that the about, version, and author fields are defined in the _Cargo.toml_ file. Also notice that the provided command is different than `cargo run --help`. We write `cargo run -- --help` to see the help message for the binary. However `cargo run --help` shows the help message for the cargo command itself. The dummy -- is used to separate the cargo command from the arguments passed to the binary. Thus, we could also write `cargo run -- reset` to run the reset command. Nevertheless, the _--_ flag is only necessary when the arguments passed to the binary are the same as the cargo command. This is to avoid ambiguity.

In the code we have `rename-all = "kebab-case"`. This is used to convert the names of the commands and the arguments to kebab-case. This means that the names are converted to lowercase and separated by hyphens. This is useful because the names of the structs and enum in Rust are typically written in camel case. However, the command line arguments are typically written in kebab case. So for example the _Reset_ option of the _Command_ enum is internally converted to _reset_.

---

We have mainly talked about the executable binary crate and the pipeline. The structure of the __rusmart-cli__ _package_ consists of two _crates_: a _binary_ crate and a _library_ crate. Note that a package is a directory that contains a _Cargo.toml_ file. A package can have multiple binary crates and optionally one library crate. A package must at least have one crate. The library crate is written inside the _lib.rs_ file in the _src_ directory and the binary crate is written inside the _main.rs_ file in the _src_ directory. Any additional binary crates are written inside the _src/bin_ directory. The binary crates are executable and the library crates provide functionality for other libraries. Note that a _crate_ is similar to a _library_ in other programming languages. To reiterate, the binary crate is the entry point of the CLI application, and the library crate contains the core functionality of the CLI application. Typically, packages like _rusmart-cli_ with this pattern of containing both a library and a binary crate will have just enough code in the binary crate to start an executable that calls code within the library crate. This lets other projects benefit from most of the functionality that the package provides because the library crate’s code can be shared. The _module tree_ should be defined in _src/lib.rs_. Then, any _public_ items can be used in the binary crate by starting paths with the name of the package. The binary crate becomes a user of the library crate just like a completely external crate would use the library crate: it can only use the public API. The contents of the library crate are as follows:

```rust

mod git;
mod dep;
mod dep_cvc5;
mod dep_z3;

pub mod cli;
```

As it can be seen the module tree system is provided. The _cli_ module is public and the other modules are private. All the private modules can only be used within the library crate. The _cli_ module is the public API of the library crate, where the functionality of the CLI application is defined and can be used by external crates along with the binary crate.

---

- **git Module**: The _git_ module is located in the _src/git.rs_ file. The _GitRepo_ struct is defined in this module. The _GitRepo_ struct encapsulates and connects the path to a Git repository and the corresponding commit hash. It provides the following functionality:
	- Create a new instance of _GitRepo_.
	- Retrieve the current commit hash of the repository.
	- Clone & Checkout the repository into a new directory.

- **cli Module**: The _cli_ module is located in the _src/cli.rs_ file. The subcommands are defined in this module. The subcommands are as follows:
	- Layer 1: The _Command_ enum providing _Reset_, _Deps_, and _Help_ options.
	- Layer 2: The _DepArgs_ struct providing _Z3_ and _CVC5_ subcommands. This is used to manage dependencies for the _Deps_ layer 1 command.
	- Layer 3: The _DepAction_ enum providing _Config_ and _Build_ options. This is used to manage the configuration and building of the dependencies for the _DepArgs_ layer 2 command. The _Build_ can be forced by using the _--force_ flag.

We execute the costumized command by loosely running the following: `cargo run <Layer 1> <Layer 2> <Layer 3>`. For example, `cargo run deps z3 build --force`.

- **dep Module**: The _dep_ module is located in the _src/dep.rs_ file. The _Dependency_ trait, the _Artifact_ struct, the _Scratch_ struct, the _Package_ struct, and the _DepState_ enum are defined in this module.
	- The _Dependency_ trait is used to mark a dependency in the project. Z3 and CVC5 are the dependencies that implement the _Dependency_ trait. It has three functions:
		- `repo_path_from_root`: This function returns the location of the git repo from the project root.
			- for Z3: `deps/z3`
			- for CVC5: `deps/cvc5`
		- `list_configurations`: This function lists configurable options for building.
		- `build`: This function builds the dependency from scratch.
	- The _Artifact_ struct is a marker over a path indicating that this is an artifact of a dependency. It has three fields:
		- `base`: The base directory.
		- `src`: The source directory which is a subdirectory of the base directory.
		- `dst`: The destination directory which is a subdirectory of the base directory.
	- The _Scratch_ struct represents the build-from-scratch state. It has three fields:
		- `repo`: The git repo.
		- `artifact`: The path to the artifact where the source and destination directories will be created.
		- `_phantom`: A PhantomData. This is used to differentiate between the functions of CV5 and Z3.
		- The _Scratch_ has a `make` function that creates subdirectories _src_ and _dst_ in the artifact path (studio/native/deps/z3/<commit hash> or studio/native/deps/cvc5/<commit hash>). Then it clones the git repository into the _src_ subdirectory. Then it builds the dependency. It returns a _Package_ struct.
	- The _Package_ struct represents the package-ready state. It has three fields:
		- `repo`: The git repo.
		- `artifact`: The artifact; which encapsulates the base, source, and destination directories.
		- `_phantom`: A PhantomData.
		- The _Package_ struct has a `destroy` function that removes the artifact directory and returns a _Scratch_ struct.
	- The _DepState_ enum automatically differentiates the scratch and package version of LLVM. It has two variants:
		- `Scratch`: The scratch state.
		- `Package`: The package state.
		- The _DepState_ enum has three functions:
			- `new`: Get the deps state.
			- `list_configurations`: List the possible build options.
			- `build`: Build the package.

- **dep_cvc5 Module**: The _dep_cvc5_ module is located in the _src/dep_cvc5.rs_ file. The _DepCVC5_ struct is defined in this module. The _DepCVC5_ struct implements the _Dependency_ trait.

- **dep_z3 Module**: The _dep_z3_ module is located in the _src/dep_z3.rs_ file. The _DepZ3_ struct is defined in this module. The _DepZ3_ struct implements the _Dependency_ trait.

---

### Testing

We have written unit tests for the _rusmart-cli_ package. The coverage of the tests is _55.67%_. The reasons for the low coverage are as follows:
	- Some of the code is not practically reachable and is not tested.
	- The code containing _make_ and _build_ functions are computationally expensive and time-consuming to test. Therefore, we have not tested them.
