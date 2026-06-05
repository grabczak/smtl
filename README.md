# SMTL

My custom language for writing SMT (Satisfiability Modulo Theories) problems.

## Setup

Install GHC, Stack and Cabal via [GHCup](https://www.haskell.org/ghcup/).

You can verify the installation by running:

```
$ ghcup tui
```

Additionally, Z3 needs to be installed on your system. Refer to build instructions on the [Z3 GitHub repository](https://github.com/z3prover/z3).

### Running the Program

To run the program in development mode:

```
$ stack run <path_to_input_file>
```

### Building a Release

To compile an optimized version of the executable:

```
$ stack build
```

### Running Tests

To execute the test suite:

```
$ stack test
```

## Working Example

```
# Comment

# Theory: QF_LIA or QF_NIA
set logic QF_LIA

# Declarations
var p, q : bool

# Assignments
let f = (p \/ ~p) /\ (F => T)
let g = (q /\ ~q) \/ (p <=> q)

# Assertions
assert f /\ g

# Output commands
check sat
get model
exit
```

```
# Theory: QF_LIA or QF_NIA
set logic QF_NIA

# Declarations
var a, b, x, y, d : int

# Assignments
let bezout = a * x + b * y == d

# Assertions
assert a == 12 /\ b == 18
assert d >= 0
assert a % d == 0 /\ b % d == 0
assert bezout

# Output commands
check sat
get model
exit
```

## Syntax

### Variable Declaration

Use `var` to declare variables with their types:

```
var p, q : bool
var a, b, x, y, d : int
```

### Expressions

#### Logical Operators

| Operator | Meaning     | Example   |
| -------- | ----------- | --------- |
| `~`      | Negation    | `~p`      |
| `\/`     | Alternative | `p \/ q`  |
| `/\`     | Conjuction  | `p /\ q`  |
| `=>`     | Implication | `p => q`  |
| `<=>`    | Equivalence | `p <=> q` |

#### Arithmetic Operators

| Operator | Meaning          | Example |
| -------- | ---------------- | ------- |
| `-`      | Negation         | `-a`    |
| `\|•\|`  | Absolute value   | `\|a\|` |
| `+`      | Addition         | `a + b` |
| `-`      | Subtraction      | `a - b` |
| `*`      | Multiplication   | `a * b` |
| `/`      | Integer division | `a / b` |
| `%`      | Modulo           | `a % b` |

#### Comparison Operators

| Operator | Meaning          | Example  |
| -------- | ---------------- | -------- |
| `=`      | Equality         | `a = b`  |
| `!=`     | Inequality       | `a != b` |
| `<`      | Less than        | `a < b`  |
| `>`      | Greater than     | `a > b`  |
| `<=`     | Less or equal    | `a <= b` |
| `>=`     | Greater or equal | `a >= b` |

### Variable Assignment

Use `let` to assign an expression to an identifier:

```
let excluded_middle = p \/ ~p
let bezout_identity = a * x + b * y == d
```

### Assertions

Use `assert` to ensure that an expression must be true:

```
assert excluded_middle
assert bezout_identity
```

## Implementation

SMTL consists of three main modules:

- **Parser** - Converts source code into an abstract syntax tree (AST)
- **Type Checker** - Validates type correctness and catches semantic errors
- **Solver** - Translates AST into SMT-LIB format

A separate Z3 process is spawned to solve the generated SMT-LIB problem.

## Warning

This is not a production-ready tool. It is a personal project for learning purposes, and as such, it may contain bugs and is not optimized for performance.

## License

BSD-3-Clause © [grabczak](https://github.com/grabczak) 2026
