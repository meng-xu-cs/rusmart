### Utilities

Contains the utility functions and modules that are shared across the project. These utilities are used in the _cli_, _lang/demo_, _lang/rego_, _smt/stdlib_, _sm/remark_, _smt/derive_, and _smt/testing_ crates. The _utils_ crate is not a binary crate, meaning it does not have a _main_ function. Instead, it is a library crate that provides reusable code. In reality, _rusmart-utils_ is a _package_ containing only one library crate. The _Cargo.toml_ file in the _utils_ directory is as follows:

```
[package]
name = "rusmart-utils"
description = "Utility functions shared among Rusmart crates"
version = "0.1.0"
edition = "2021"
authors = ["Meng Xu <meng.xu.cs@uwaterloo.ca>"]
license = "GPL-3.0"

[dependencies]
lazy_static = { workspace = true }
log = { workspace = true }
num_cpus = { workspace = true }
simplelog = { workspace = true }
```

As seen, the _name_, _description_, _version_, _edition_, _authors_, and _license_ fields are set to the appropriate values. The _rusmart-utils_ crate depends on the _lazy_static_, _log_, _num_cpus_, and _simplelog_ crates. If we do not include these dependencies in the _Cargo.toml_ file of the _utils_ crate, the compiler will not be able to find them when building the _utils_ crate. The _src_ directory contains the source code of the utility functions and modules. This directory contains two files, _lib.rs_ and _config.rs_. The _lib.rs_ is the root of the crate, which in this case just contains the module tree system making the _config_ module publicly available to external crates. This is useful as the _rusmart-cli_ crate uses the functionalities provided in this module. The _config.rs_ file contains the configuration settings for the project. Lets walk through the _config.rs_ source code:

- We first define static variables `PROJECT` and `INITIALIZED` as `&str` and `AtomicBool` types with initial values of `"RUSMART"` and  `AtomicBool::new(false)`, respectively. Note that the `AtomicBool` type is a boolean type that can be safely shared between threads.

- We have four modes of operations: `Prod`, `Dev`, `Verbose`, and `Debug`. We implement the `Display` trait for the `Mode` enum containing the four variants.

- In Rust, the `lazy_static!` macro is used to define static variables that are initialized at runtime rather than compile time. The `ref` keyword is used within the `lazy_static!` macro to declare a lazily-initialized static reference. The `MODE` variable is set to `Mode::Dev` by default or if we define the `V`, `Verbose`, or `RUSMART_VERBOSE` environment variable as 1. If we define the environment variable as zero, the `MODE` variable is set to `Mode::Prod`. If we set them as 2, the `MODE` variable is set to `Mode::Debug`. The `MODE` variable is set to `Mode::Verbose` if we set the `V`, `Verbose`, or `RUSMART_VERBOSE` environment variable as 3 or higher. We can set the environment variables by running the `cargo build` command like `RUSMART_VERBOSE=3 cargo build`. Note that `RUSMART_VERBOSE` has priority over `VERBOSE` which has priority over `V`.

- A `workspace` is a struct with `base` and `studio` fields. Each field is a `PathBuf type`, which is an owned, mutable path, unlike `Path` types which are immutable, borrowed references to a path. The `WKS` is a `Workspace` type that contains the `base` and `studio` fields. The `base` field is set to the path of the workspace directory (specifically where the `Cargo.toml` file is located). The `studio` is the _path-to-workspace/studio/native_ or _path-to-workspace/studio/docker_ depending on whether the `dockerized` variable is set to true or false; i.e., whether the `DOCKER` environment variable was set to 1 or not. Note that there is a difference between `let dockerized = matches!(env::var("DOCKER"), Ok(val) if val == "1");` and `let dockerized = matches!(env::var("DOCKER"), Ok("1"));`. The latter will always fail to match because it tries to directly match a String (returned by env::var) with a string slice ("1"). Therefore, dockerized will always be false.

- In the `initialize` function, we check if the `INITIALIZED` variable is set to `true`. If it is, we return early. Otherwise, we set the `INITIALIZED` variable to `true`. This is done using the `compare_exchange` method of the `AtomicBool` type; this method ensures thread safety. The `compare_exchange` method atomically compares the value of the `AtomicBool` variable with the expected value and, if they are not equal, sets the value of the `AtomicBool` variable to the new value. If the values are equal (meaning INITIALIZED is true already), the method returns an `Err` value. The `compare_exchange` method is used to ensure that the `INITIALIZED` variable is only set to `true` once.

Moreover, depending on the `MODE`, which can be either the `Prod`, `Dev`, `Debug`, and `Verbose` modes; the level of logging is defined as `Warn`, `Info`, `Debug`, or `Trace` respectively. We then define a configuration builder that allows us to set the log level for different parts of the project. The `ConfigBuilder` struct has four fields: `location_level`, `target_level`, `thread_level`, and `time_level`. By setting the LevelFilter to Off for each one, we disable logging of source code locations (e.g., file names, line numbers), disable logging of the target, disable logging of thread information (which thread is generating the log message), and disable logging of timestamps, respectively. We then intialize our costumed logger, `Terminal Logger`, with the level of logging, configuration, terminal mode (`mixed` means both `stderr` and `stdout` are used for printing the logs), and ColorChoice (it is set to `Auto` which means the log messages are displayed in colour if the terminal allows it) as its fields. If any of the fields are not initialized a panic with the following message will occur `logging facility should be initialized`.


#### Testing

Testing the utils crate is not necessary because it is a utility crate and does not implement the logic of our rusmart tool. However, we can write unit tests for the _config.rs_ file to ensure that the configuration settings are correct. Note that because most of the variables defined in the config.rs file are _lazy static_ variables, testing their values at compile time comes with certain challenges. Nevertheless, unit tests were written with 69.23% statement coverage by the _cargo tarpaulin_ tool.