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
```