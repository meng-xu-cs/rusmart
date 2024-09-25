### Remark 

##### cargo toml

In Rust, the name of a package is often written in kebab-case (i.e., with hyphens) in Cargo.toml (for example _name = "rusmart-smt-remark"_ under the _[package]_ section). The name mentioned in the Cargo.toml of the workspace uses the kebab-case as well; _rusmart-smt-remark = { path = "smt/remark" }_, but when you use it in the code, Rust converts it to snake_case (i.e., with underscores). Therefore, to import the library crate from the package, you need to write _use rusmart\_smt\_remark;_.

Initially, we will start explaining the _Cargo.toml_ file. This file is used to define the metadata of the package, where it is similar to the _Cargo.toml_ file of the _rusmart-cli_, _rusmart-utils_, and _rusmart-smt-stdlib_ packages. However, the file contains the following dependencies:

```toml
[package]
... # Omitted for brevity

[lib]
proc-macro = true

[dependencies]
proc-macro2 = { workspace = true }
quote = { workspace = true }
syn = { workspace = true }

[lints.rust]
unexpected_cfgs = { level = "warn", check-cfg = ['cfg(tarpaulin_include)'] }
```

The _lib_ section indicates that the package is a library crate and contains a __procedural macro__. The _proc_macro_ crate is the compiler’s API that allows us to read and manipulate Rust code from our code. The _syn_ crate parses Rust code from a string into a data structure that we can perform operations on. The _quote_ crate turns syn data structures back into Rust code. These crates make it much simpler to parse any sort of Rust code we might want to handle. The versions used according to the _Cargo.toml_ of the workspace are:

- proc-macro2 = "1.0.86"
- quote = "1.0.36"
- syn = { version = "2.0.72", features = ["full", "extra-traits"] }

The _full_ feature of the _syn_ crate is used to enable _Data structures for representing the syntax tree of all valid Rust source code, including items and expressions_. The _extra-traits_ feature is used to make the _Debug, Eq, PartialEq, Hash impls for all syntax tree types_ available. The syntax tree type is the result of parsing the Rust code.

The _lints.rust_ section is only defined for the _Cargo.toml_ of a package. The _unexpected_cfgs_ lint is used to check for unexpected _cfg_ attributes in the code. This flag allows using __#[cfg(not(tarpaulin_include))]__ in the code to exclude the code (function) from the coverage report, without generating a warning. We have explicitly excluded the procedural macro functions from the coverage report, as unit tests are not applicable to them.

---

##### modules

- __err__ : The _err_ module provides a set of error-handling utilities, specifically designed for use within procedural macros. It includes five macros to simplify compilation error generation or error handling when working with Rust's macro system. The fail_on! and fail_if_error! macros are used to generate compiler errors. The bail_on!, bail_if_exists!, and bail_if_missing! macros return error instances for later use.

- __attr__ : The _attr_ module provides utilities for parsing key-value mappings from token streams, primarily through the _parse_dict_ function. This function is used in the _derive_for_func_ function, which in turn is invoked by the _smt_impl_ and _smt_spec_ macros in the lib module. The parse_dict function takes a token stream and extracts key-value pairs, for example #[method = add] => Map {method : add}. It returns a map of the parsed keys and their associated values, while also handling errors related to missing keys, invalid syntax, and duplicate entries.

- __ty__ : The provided module is responsible for generating implementations of the SMT trait for Rust structs and enums using procedural macros. This module includes functions like derive_for_struct and derive_for_enum, which parse and inspect the fields and variants of structs and enums, respectively, to generate trait implementations. The SMT trait appears to define a custom comparison method (_cmp) and, for enums, it also derives the Default trait based on the first variant. It also includes error handling utilities to gracefully manage unexpected attributes, mutability declarations, and missing or invalid struct or enum definitions.

_ __generics__ : This module provides utilities for parsing and managing generic type parameters in Rust code. The central structure, _TypeParamGroup_, is responsible for collecting and handling type parameters from generic definitions. It provides methods to parse generic parameters, check for specific parameters, collect type arguments from types, and convert the parameters into syntax suitable for various use cases like definitions, usage, and function invocation. The module ensures that the generics adhere to specific rules, such as requiring the SMT trait for each type parameter and preventing invalid constructs like default type parameters or lifetimes in certain contexts.

- __func__ : The _func_ module provides functionality for deriving annotations for functions used in procedural macros. It includes two key functions: _derive_for_impl_ and _derive_for_spec_, which are used by the smt_impl and smt_spec macros, respectively. The derive_for_impl function is responsible for generating code based on implementation annotations, while derive_for_spec handles specification annotations. Both functions validate the provided function's signature, ensuring it complies with certain rules, such as no use of const, async, unsafe, or variadic parameters, and enforcing that all generics implement the SMT trait. Additionally, the _check_and_derive_ function generates method implementations when provided with a method attribute, deriving new methods for designated types.

- __lib__ : This library module defines procedural macros for annotating Rust types, functions, and constants in order to generate additional code and derive traits. It provides the module syntax tree (attr, err, func, generics, and ty) and defines four procedural macros: __smt_type, smt_impl, smt_spec, and smt_axiom__.
    - smt_type is used to annotate Rust type definitions (like structs and enums), automatically deriving traits such as Clone, Debug, and SMT.
    - smt_impl and smt_spec annotate top-level functions to derive code for implementation and specification annotations, respectively.
    - smt_axiom is used to annotate constants, specifically enforcing that only functions are used with this annotation.

---

#### Testing

We have written unit tests for the _rusmart-smt-remark_ package. The coverage of the tests is _89.93%_. The reasons for the missing coverage are:
    - The functions that have proc-macro TokenStream as an argument are not covered by the unit tests as we cannot test because procedural macro API is used outside of a procedural macro error. 
The coverage for modules is as follows:
    - The coverage for the _attr_ module is _100%_.
    - The coverage for the _generics_ module is _99.10%_.
    - The coverage for the _ty__ module is _93.12%_.
    - The coverage for the _func_ module is _64.56%_. This is because the functions that have proc-macro TokenStream as an argument are present in this module.
    - The _err_ and _lib_ do not have any functions to test.
