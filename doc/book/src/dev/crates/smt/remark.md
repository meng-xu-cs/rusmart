Initially, we will start with explaining the _Cargo.toml_ file. This file is used to define the metadata of the package, where it is similar to the _Cargo.toml_ file of the _rusmart-cli_, _rusmart-utils_, and _rusmart-smt-stdlib_ packages. However, the file contains the following dependencies:

```toml
[package]
... # Omitted for brevity

[lib]
proc-macro = true

[dependencies]
proc-macro2 = { workspace = true }
quote = { workspace = true }
syn = { workspace = true }
```

The _lib_ section indicates that the package is a library crate and contains a __procedural macro__. The _proc_macro_ crate is the compiler’s API that allows us to read and manipulate Rust code from our code. The _syn_ crate parses Rust code from a string into a data structure that we can perform operations on. The _quote_ crate turns syn data structures back into Rust code. These crates make it much simpler to parse any sort of Rust code we might want to handle