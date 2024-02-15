# Annotations

Rusmart is essentially a subset of Rust language plus annotations
to bridge the semantic gap between Rust and SMT.

Internally, annotations are implemented
as [procedural macros](https://doc.rust-lang.org/reference/procedural-macros.html)
in the `rusmart-smt-remark` crate.
However,
due to the complexity of Rusmart [syntax](syntax.md) and [typing](typing.md) rules,
the `rusmart-smt-remark` crate performs minimal checks only and
more intensive checking are performed in the `rusmart-smt-derive` crate.

## `#[smt_type]`

This annotation can only be applied on a type.