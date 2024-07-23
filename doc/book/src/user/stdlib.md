# stdlib

Rusmart standard library (short for `stdlib`) contains
language constructs that cannot be expressed readily in Rust
as they have special semantics in SMT.

## Trait

- `SMT`: marks that a Rust type is also an SMT type.
  In order to bridge the semantic gap between a Rust type and an SMT type,
  the `SMT` trait encodes several restrictions:

    - Rust types implementing the `SMT` trait
      must also implement other traits, including:
        - `Copy`: as SMT values are processed by value and never by reference.
        - `Ord`: such that the SMT value can be placed in a `BTreeMap` as key
          (note: `BTreeMap` is behind the `Map` data type).
        - `Default`: to allow quantified expressions
          (including `choose` operators)
          to type-check in Rust type system.
          The implementation can be arbitrary (including `panic!`)
          as it won't be executed concretely.

    - Rust types implementing the `SMT` trait
      will also need to implement the following functions
      that are generally supported on SMT values:
        - `eq`: equality test
        - `ne`: non-equality test

## Data types

These data types are part of the [type system](typing.md) in Rusmart:

- `Boolean`
- `Integer`
- `Rational`
- `Text`
- `Cloak<T>`
    - A wrapper over `T` to allow recursive data types to be defined
      (similar to `Box<T>` in Rust).
      A `Cloak<T>` will be uncloaked to `T` after the parsing stage of Rusmart.
- `Seq<T>`
    - SMT sequence of type `T`.
- `Set<T>`
    - SMT set of type `T`.
- `Map<K, V>`
    - SMT array of key type `K` and value type `Option<V>`
      with `None` as the default values.
      This is to support the modeling on which key may exist.
- `Error`
    - A special marker to indicate error states.

## Expressions

- `forall |v1: T1, v2: T2, ..., vn: Tn| <predicate>(v1, v2, ..., vn)`
    - **SMT only**: universally quantified over potentially unbounded types

- `forall |v1 in c1, v2 in c2, ..., vn in cn| <predicate>(v1, v2, ..., vn)`
    - **SMT and Rust**: universally quantified over bounded collections
    - In Rust, the `<predicate>` are checked in a loop
      iterating over all possible combination of variables `v1, v2, ..., vn`.

- `exists |v1: T1, v2: T2, ..., vn: Tn| <predicate>(v1, v2, ..., vn)`
    - **SMT only**: existentially quantified over potentially unbounded types

- `exists |v1 in c1, v2 in c2, ..., vn in cn| <predicate>(v1, v2, ..., vn)`
    - **SMT and Rust**: existentially quantified over bounded collections
    - In Rust, the `<predicate>` are checked in a loop
      iterating over all possible combination of variables `v1, v2, ..., vn`.

- `choose |v1: T1, v2: T2, ..., vn: Tn| <predicate>(v1, v2, ..., vn)`
    - **SMT only**: choose operator over potentially unbounded types.
    - In SMT< Variables `v1, v2, ..., vn` will be defined in an axiomatized way.

- `choose |v1 in c1, v2 in c2, ..., vn in cn| <predicate>(v1, v2, ..., vn)`
    - **SMT and Rust**: choose operator over bounded collections
    - In Rust, one set of variables `v1, v2, ..., vn`
      that satisfies `<predicate>` will be returned
      by iterating over all possible combinations.
      If no such combination exists, exit with panic.
    - In SMT, variables `v1, v2, ..., vn` will be defined in an axiomatized way.