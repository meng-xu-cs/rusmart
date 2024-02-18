## Naming

There are four types of named items in Rusmart:

- `#[smt_type]`
- `#[smt_impl]`
- `#[smt_spec]`
- `#[smt_axiom]`

Each item should have a unique name.
Namespace is not supported at this moment.

As `#[smt_type]` is also a Rust type while
`#[smt_impl]`, `#[smt_spec]`, `#[smt_axiom]` are Rust functions,
their names should also follow the naming convention of Rust and functions, respectively.