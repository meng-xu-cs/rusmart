# Annotations

Rusmart is essentially a subset of Rust language plus annotations
to bridge the semantic gap between Rust and SMT.

Internally, annotations are implemented
as [procedural macros](https://doc.rust-lang.org/reference/procedural-macros.html)
in the {{#include ../dict/crate-remark.md}} crate.
However,
due to the complexity of Rusmart [syntax](syntax.md) and [typing](typing.md) rules,
the {{#include ../dict/crate-remark.md}} crate performs minimal checks only and
more intensive checking are performed in the {{#include ../dict/crate-derive.md}} crate.

In this document,
we only discuss the checks enforced by {{#include ../dict/crate-remark.md}}.
A violation of these checks will lead to compile-time error.

## `#[smt_type]`

- This annotation can only be applied on a Rust type definition
  i.e., `struct` and `enum`.

- Attributes will not be accepted.
  i.e., `#[smt_type(<...attrs...>)]` will cause an error.

## `#[smt_impl]` and `#[smt_spec]`

- This annotation can only be applied on a Rust function
  i.e., `fn` as top-level module item
  (in contrast to `fn` in an `impl` block)

- Allowed attributes include:
    - `method = <ident>`: will derive a method
      with the type of the first parameter as the receiver (i.e., `self`).

If method derivation is requested,
additional requirements apply:

- The `fn` cannot be `const`, `async`, `unsafe`, `extern`,
  nor can it be variadic.

- The first parameter cannot be a receiver
  (e.g., `self`, `&self`, `&mut self`).

- The type of the first parameter (denoted as `T`) is not qualified.

- If `T` has type arguments, all <TODO>

- If the underlying `fn` is generic,
  only type parameter is allowed in the generics declaration and
  the type parameter must implement the `SMT` trait (see [`stdlib`](stdlib.md)).

## `#[smt_axiom]`