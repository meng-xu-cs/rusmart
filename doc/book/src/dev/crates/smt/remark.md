### Remark 

##### cargo toml

In Rust, the name of a package is often written in kebab-case (i.e., with hyphens) in Cargo.toml (for example _name = "rusmart-smt-remark"_ under the _[package]_ section). The name mentioned in the Cargo.toml of the workspace uses the kebab-case as well; _rusmart-smt-remark = { path = "smt/remark" }_, but when you use it in the code, Rust converts it to snake_case (i.e., with underscores). Therefore, to import the library crate from the package, you need to write _use rusmart\_smt\_remark;_.

Initially, we will start explaining the _Cargo.toml_ file. This file is used to define the metadata of the package, where it is similar to the _Cargo.toml_ file of the _rusmart-cli_, _rusmart-utils_, and _rusmart-smt-stdlib_ packages. However, the file contains the following dependencies:

```toml
[package]
... # Omitted for brevity

[dependencies]
proc-macro2 = { workspace = true }
quote = { workspace = true }
syn = { workspace = true }
trybuild = { workspace = true }
```

The versions used according to the _Cargo.toml_ of the workspace are:

- proc-macro2 = "1.0.86"
- quote = "1.0.36"
- syn = { version = "2.0.72", features = ["full", "extra-traits"] }
- trybuild = "1.0"

The _full_ feature of the _syn_ crate is used to enable _Data structures for representing the syntax tree of all valid Rust source code, including items and expressions_. The _extra-traits_ feature is used to make the _Debug, Eq, PartialEq, Hash impls for all syntax tree types_ available. The syntax tree type is the result of parsing the Rust code. _Trybuild_ is a test harness for invoking rustc on a set of test cases and asserting that any resulting error messages are the ones intended. More information about the _trybuild_ crate can be found [here](https://docs.rs/trybuild/latest/trybuild/).

---

##### modules

- __err__ : The _err_ module provides a set of error-handling utilities, specifically designed for use within procedural macros. It includes five macros to simplify compilation error generation or error handling when working with Rust's macro system. The _fail\_on!_ and _fail\_if\_error!_ macros are used to generate compiler errors. The _bail\_on!_, _bail\_if\_exists!_, and _bail\_if\_missing!_ macros return error instances for later use.

- __attr__ : The _attr_ module provides utilities for parsing key-value mappings from token streams, through the _parse\_dict_ function. This function is used in the _derive\_for\_func_ function, which in turn is invoked by the _smt\_impl_ and _smt\_spec_ macros in the lib module of the _rusmart\_smt\_remark\_derive_ crate. The parse_dict function takes a token stream and extracts key-value pairs, for example #[my_attr(method = add)] => Map {method : add}. It returns a map of the parsed keys and their associated values, while also handling errors related to missing keys, invalid syntax, and duplicate entries.

- __ty__ : The provided module is responsible for generating implementations of the SMT trait for Rust structs and enums using procedural macros. This module includes functions like _derive\_for\_struct_ and _derive\_for\_enum_, which parse and inspect the fields and variants of structs and enums, respectively, to generate trait implementations. To implement the SMT trait, the comparison method (_cmp) is defined. Moreover, for enums, it also derives the Default trait based on the first variant. It also includes error handling utilities to manage unexpected attributes and missing or invalid struct or enum definitions.

_ __generics__ : This module provides utilities for parsing and managing generic type parameters in Rust code. The central structure, _TypeParamGroup_, is responsible for collecting and handling type parameters from generic definitions. It provides methods to parse generic parameters, check for specific parameters, collect type arguments from types, and convert the parameters into syntax suitable for various use cases like definitions, usage, and function invocation. The module ensures that the generics adhere to specific rules, such as requiring the implementation of the SMT trait for each type parameter and preventing invalid constructs like default type parameters or lifetimes.

- __func__ : The _func_ module provides functionality for deriving annotations for functions used in procedural macros. It includes two key functions: _derive\_for\_impl_ and _derive\_for\_spec_, which are used by the _smt\_impl_ and _smt\_spec_ macros, respectively. Additionally, the _check_and_derive_ function is called inside the _derive\_for\_impl_ and _derive\_for\_spec_ functions to verify the function's signature and generate the appropriate code - ensuring it complies with certain rules, such as no use of const, async, unsafe, or variadic parameters, and enforcing that all generics implement the SMT trait. The primary purpose of the functions (derive_for_impl and derive_for_spec) is to implement the _provided method as an argument_ for the first parameter of the function argument.

- __lib__ : This library module defines the module tree system. Accordingly, the _rusmart\_smt\_remark_ package contains one library crate. The module tree structure is as follows:
    - attr, generics, err, func, ty
    - _func_ and _ty_ are public modules and can be accessed from outside the crate. The mentioned modules are used in the _rusmart\_smt\_remark\_derive_ package.

---

#### Testing

We have written unit tests for the _rusmart-smt-remark_ package. The coverage of the tests is _98.80%_. The coverage for modules is as follows:
    - The coverage for the _attr_ module is _100%_.
    - The coverage for the _generics_ module is _100%_.
    - The coverage for the _ty__ module is _97.87%_.
    - The coverage for the _func_ module is _98.72%_.
    - The _err_ and _lib_ modules do not have any functions to test.

Note that compared to the original design, the functions that have proc-macro TokenStream as an argument have been changed to work with the proc-macro2 TokenStream. Therefore, internally, no procedural macro API is used outside of a procedural macro and all the functions are covered by the unit tests. Also, the procedural macros have been embedded in the _rusmart\_smt\_remark\_derive_ package. The reasons for the missing coverage are:
    - In the _func_ module, the path `bail_if_exists!(iter.next());` in the `check_and_derive` function is not covered by the unit tests. This is because this part is unreachable as the condition is checked in the line invoking the `collect_type_arguments` function.
    - In the _ty_ module, the four paths `bail_on!(field, "unexpected field mutability declaration"); //unreachable code as the mutability is always FieldMutability::None (for now)` are not covered by the unit tests. This is because the mutability is always _FieldMutability::None_.
    - In the previous design, the functions that had proc-macro TokenStream as an argument were not covered by the unit tests as we could not test because of the _"procedural macro API is used outside of a procedural macro"_ error. This has been fixed in the current design.

---

### _rusmart\_smt\_remark\_derive_ Package

The next step is to define the procedural macros. Procedural macros need to be in their own crate. The convention for structuring crates and macro crates is as follows: for a crate named _foo_, a custom derive procedural macro crate is called _foo\_derive_. Thus, the name of the procedural macro crate for the _rusmart-smt-remark_ package is _rusmart-smt-remark-derive_. The _Cargo.toml_ file of the _rusmart-smt-remark-derive_ package is as follows:

```
[package]
... # Omitted for brevity

[lib]
proc-macro = true

[dependencies]
proc-macro2 = { workspace = true }
syn = { workspace = true }
rusmart-smt-remark = { workspace = true }

[lints.rust]
unexpected_cfgs = { level = "warn", check-cfg = ['cfg(tarpaulin_include)'] }
```

The _lib_ section indicates that the package is a library crate and contains a __procedural macro__. The _proc_macro_ crate is the compiler’s API that allows us to read and manipulate Rust code from our code. The _syn_ crate parses Rust code from a string into a data structure that we can perform operations on. The _quote_ crate turns syn data structures back into Rust code. These crates make it much simpler to parse any sort of Rust code we might want to handle. The _lints.rust_ section is only defined for the _Cargo.toml_ of a _package_ (not a workspace). The _unexpected_cfgs_ lint is used to check for unexpected _cfg_ attributes in the code. This flag allows using __#[cfg(not(tarpaulin_include))]__ in the code to exclude the code (function) from the coverage report, without generating a warning. We have explicitly excluded the procedural macro functions from the coverage report, as unit tests are not applicable to them. Last but not least, as it can be seen, this package is dependent on the _rusmart-smt-remark_ package.


This package contains one library crate, which in turn has only one _lib_ module defining procedural macros for annotating Rust types, functions, and constants in order to generate additional code and derive traits. It provides four procedural macros: __smt_type, smt_impl, smt_spec, and smt_axiom__.
    - smt_type is used to annotate Rust type definitions (like structs and enums). The primary purpose of this procedural macros is to automatically derive the __SMT__ trait for the annotated types by generating the necessary code for the comparison method (cmp) method.
    - smt_impl and smt_spec annotate top-level functions to derive code for implementation and specification annotations, respectively. The primary purpose of these procedural macros is to generate code to implement a _method_ for the first parameter of the given function.
    - smt_axiom is used to annotate functions, specifically enforcing that no attributes are present.

Regarding testing, the _lib_ module contains the procedural macros, and thus, procedural macro API cannot be used outside of a procedural macro. Therefore, the procedural macros functions are not covered by the unit tests.