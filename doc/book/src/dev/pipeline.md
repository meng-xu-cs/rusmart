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
*`P'` passes the Rust compiler* (both in syntax and in typing).
Effectively, this means that Rusmart syntax and type checking
is based on Rust (i.e., is a subset of Rust).
