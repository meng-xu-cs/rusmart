# rusmart

Programming Language | Implemented in Rust | Formulated in SMT

## Introduction

Rusmart is a Rust to Satisfiability Modulo Theories (SMT) formulae transpiler
that is implemented mostly via [Rust procedural macros](https://doc.rust-lang.org/reference/procedural-macros.html).

Rusmart is also a programming language that
empathizes on the modeling of program semantics,
in particular:

- the operational semantics of a Rusmart program is represented by Rust code
  which is concretely executable, while
- the denotational semantics of the program is seamlessly transpiled into SMT
  which can be symbolically reasoned.

## The Pitch

Formal verification provides the utmost correctness assurance to programs
while satisfiability modulo theories (SMT) solvers
(e.g., [Z3](https://github.com/Z3Prover/z3) and [CVC5](https://github.com/cvc5/cvc5))
are at the core of most *push-button style* formal verification tools.

Unfortunately, software verification requires scaffolding to

- Transpile code to SMT: `C`
- Transpile specification (i.e., correctness properties) to SMT: `S`
- Prove `S => C`

While (automated) theorem proving is an independent line of research and
has seen tremendous progress in recent years,
the transpilers are still limiting factors to the adoption of formal verification.

Rusmart aims to remove the scaffolding transpilers by
seamlessly blending operational and denotational semantics of a program.

Rusmart is based on the insight that:
**most if not all sorts of SMT formulae can be constructed in Rust**.
Therefore, we can develop both code and specification in a subset of Rust
(i.e., the fragments that we know how to transpile),
use glue macros to tie them together,
and leave the proving work to SMT solvers.

In this repository, you can find examples that elaborate this insight.
These examples showcase
how to develop complex SMT formulae with Rust syntax in Rusmart,
including:

- Generic type system (including native SMT types such as `Set` and `Array`)
- Expressions allowed (including quantified expressions to replace loops)
- Declarative constructs in imperative programs (e.g., uninterpreted function, axiomatization)

## The Rusmart Book

The Rusmart book aims to be a comprehensive documentation about both

- the Rusmart language features from a user perspective
  (e.g., type system, native expressions, etc.), and
- the internal design and implementation details of the Rusmart transpiler
  which lift a subset of Rust into SMT formulae.

With [mdBook](https://rust-lang.github.io/mdBook/) installed,
the Rusmart book can be built and viewed locally via:

```bash
make docs
```

However, as Rusmart is still under active development,
changes, even major refactorings, are constantly pushed to the codebase.
Therefore, expect the book to be incomplete for a foreseeable amount of time.
