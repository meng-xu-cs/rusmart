# Demo Language

## Language syntax

```ebnf
literals           := true | false | <nat>
variable           := <name>

operator           := + | - | * | / | == | != | > | >= | <= | < | & | | | ^

expr               := expr-leaf | expr-bin-op | expr-block
expr-leaf          := literals | variable
expr-bin-op        := expr operator expr
expr-block         := block-head expr

block-head         := {stmt}*

stmt               := stmt-assign | stmt-if
stmt-assign        := var := expr;
stmt-assign-if     := var := expr if expr;

program:           := expr
```

## Language semantics

### Type system

- `Bool`, `Int` for any defined value
- `Undef` for any undefined value