# Demo Language

## Language syntax

```ebnf
literals            := true | false | <nat>
variable            := <name>

operator            := + | - | * | / | == | != | > | >= | <= | < | & | | | ^

expr                := expr-leaf | expr-bin-op | expr-block
expr-leaf           := literals | variable
expr-bin-op         := expr operator expr
expr-block          := block-head expr

block-head          := {stmt}*

stmt                := stmt-assign | stmt-if
stmt-assign         := var := expr;
stmt-assign-if      := var := expr if expr;

program:            := expr
```

## Language semantics

### Type system

- `Bool`:
    - Literal: `true`, `false`
    - `Bool & Bool`, `Bool | Bool`, `Bool ^ Bool`
    - `Bool == Bool`, `Bool != Bool`
    - `Int > Int`, `Int >= Int`, `Int <= Int`, `Int < Int`, `Int == Int`, `Int != Int`

- `Int`:
    - Literal: `<nat>`
    - `Int + Int`, `Int - Int`, `Int * Int`
    - `Int / Int` yields either `Int` or *divide-by-zero* runtime error

- `Null`
    - `var := val if cond` when `cond` evaluates to `false`

- `Undef`
    - `var` if `var` is not previously assigned

### Example

```rego
a := 1 + 2;
b := 3 if a >= 0;
a + b

// evaluate to 6
```

```rego
a := 1 + false;
b := 3 if a >= 0;
a + b

// evaluate to fail by type mismatch
```

### Workflow

Suppose the goal is to stress-test a specific implementation of the language
interpreter `I`:

- Develop a reference implementation of the language interpreter in Rusmart,
  a subset of Rust that can be converted to SMT representations.
    - A concrete interpreter: `C`
    - A symbolic interpreter: `S`

- For each possible error code `c`:
    - Query SMT solver for `exists p: Program` s.t. `S(p) -> Error(c)`
    - Send `p` to `I` (i.e., `r <- I(p)`) and compare `r` and `c`

- For each possible non-error result `v`:
    - Query SMT solver for `exists p: Program` s.t. `S(p) -> v`
    - Send `p` to `I` (i.e., `r <- I(p)`) and compare `r` and `v`
