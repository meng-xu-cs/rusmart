# Pipeline

A Rusmart program is essentially a collection of `*.rs` files written
in the Rusmart dialect (a subset of Rust),
e.g., with Rusmart-related
[procedure macros](../user/annotations.md),
[`stdlib`](../user/stdlib.md) operations, and
following the Rusmart [syntax](../user/syntax.md) and
[type system](../user/typing.md)
(which is more restrictive than Rust).

Every Rusmart program (denoted as `P`) will first go through the
{{#include ../../dict/crate-remark.md}} crate to
enrich its abstract syntax tree (AST).
The AST-extended program is a new Rust program (denoted as `P'`)
which **can be concretely executed** if `P` is well-formed.
(However, `P` itself may or may not be concretely executable.)

Derivation of the denotational semantics of `P`
occurs in the {{#include ../../dict/crate-derive.md}} crate
which will, in theory, re-check every property
that is checked in the {{#include ../../dict/crate-remark.md}} crate.
with the additional knowledge that
`P'` **passes the Rust compiler** (both in syntax and in typing).
Effectively, this means that Rusmart syntax and type checking
is based on Rust (i.e., is a subset of Rust).

## AST Enrichment

The logic about AST enrichment is encapsulated
in the {{#include ../../dict/crate-remark.md}} crate.
Briefly, a Rusmart program is enriched with the following AST fragments:

### On types

- All `struct` types annotated with `#[smt_type]` will be instrumented with
  `#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]`

- All `enum` types annotated with `#[smt_type]` will be instrumented with
  `#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]` and
  the `Default` trait will be implemented for the type
  (the first enum variant will be constructed using default values).

- The `SMT` trait from [`stdlib`](../user/stdlib.md) will be implemented
  for all types annotated with `#[smt_type]`

To illustrate the AST enrichment on types:

```rust
#[smt_type]
enum MyEnum {
    V0 { a: MyTypeA, b: MyTypeB },
    V1,
    V2(MyTypeC)
}

// enriched AST
#[smt_type]
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
enum MyEnum {
    V0 { a: MyTypeA, b: MyTypeB },
    V1,
    V2(MyTypeC)
}

impl Default for MyEnum {
    fn default() -> Self {
        Self::V0 { a: MyTypeA::default(), b: MyTypeB::default() }
    }
}

impl SMT for MyEnum {}
```

### On functions

- If a function `f` is annotated with either `#[smt_impl]` or `#[smt_spec]` and
  the `method = <method_name>` attribute is specified, the receiver-style
  method is derived for this function with an `impl` block on the type of
  the first argument of `f`. In other words, in the derived method, `self`
  is the first argument of `f`.

To illustrate the AST enrichment on functions:

```rust
#[smt_impl(method = my_method)]
fn my_func(x: MyTypeA, y: MyTypeB) -> MyTypeC { .. }

// enriched AST
#[smt_impl(method = my_method)]
fn my_func(x: MyTypeA, y: MyTypeB) -> MyTypeC { .. }

impl MyTypeA {
    fn my_method(self, y: MyTypeB) -> MyTypeC { my_func(self, y) }
}
```

## SMT Derivation

The logic about SMT derivation is encapsulated
in the {{#include ../../dict/crate-derive.md}} crate,
which can be further decomposed into three steps:

### Step 1: AST parsing and syntax checking

Source code location: `src/parser`

While the exact AST parsing process is better read from the code,
here is a brief outline of the parsing chain
(by chasing the typestate of the parsing context as in `ctxt.rs`):

- Collect all Rusmart code for the program `P`
    - `&Path -> Context`
- Collect generics in type definitions
    - `Context -> ContextWithGenerics`
- Build a type registry for `P` by parsing all type definitions
    - `ContextWithGenerics -> ContextWithType`
- Collect all function signatures
    - `ContextWithType -> ContextWithSig`
- Build a function registry for `P` by parsing all function definitions
    - `ContextWithSig -> ContextWithFunc`
- Finalize the context
    - `ContextWithFunc -> ASTContext`

**TODO**: not fully tested

### Step 2: Type checking and IR validation

Source code location: `src/ir`

**TODO**: not documented

**TODO**: not tested

### Step 3: IR analysis and SMT generation

Source code location: `src/analysis` and `src/backend`

**TODO**: not yet implemented