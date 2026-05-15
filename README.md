# SMTL

My custom language for writing SMT (Satisfiability Modulo Theories) problems.

## Setup

Install GHC, Stack and Cabal via [GHCup](https://www.haskell.org/ghcup/).

You can verify the installation by running:

```bash
$ ghcup tui
```

Additionally, Z3 needs to be installed on your system. Refer to build instructions on the [Z3 GitHub repository](https://github.com/z3prover/z3).

### Running the Program

To run the program in development mode:

```bash
$ stack run <path_to_input_file>
```

### Building a Release

To compile an optimized version of the executable:

```bash
$ stack build
```

### Running Tests

To execute the test suite:

```bash
$ stack test
```

## Working Example

```
# Comment

# Declarations
var p, q : bool
var m, n : int

# Assignments
let f = (~p \/ q) /\ (p <=> q) /\ (F => T)
let g = -m * m + n - 1 < -1

# Assertions
assert f
assert g
```

## SMT Overview

SMT formulas represent a problem: given constraints over typed variables (booleans, integers, etc.), find values that satisfy all of them at once.
You can think of SMT problems as extensions of SAT problems, where instead of just true/false variables, you have richer types and operations.

Here's an example:

```
Find booleans p, q and integers x, y such that:
  p ∨ q
  x > 0 ∧ y > 0
  x · y = 12
  x < y
```

Such problems can be solved by solvers such as Z3 or CVC5, which take SMT-LIB files as an input.
Here is the SMT-LIB for the problem above:

```
(declare-const p Bool)
(declare-const q Bool)
(declare-const x Int)
(declare-const y Int)
(assert (or p q))
(assert (and (> x 0) (> y 0)))
(assert (= (* x y) 12))
(assert (< x y))
```

SMT-LIB uses a Lisp-like syntax, and while it's easy to parse, it can be difficult to write by hand.
SMTL tries to solve this problem by providing a more user-friendly syntax, which is then translated to SMT-LIB under the hood.

```
var p, q : bool
var x, y : int

assert p \/ q
assert x > 0 /\ y > 0
assert x * y == 12
assert x < y
```

As of today, SMTL supports booleans and integers.
All computations are performed in the theory of Quantifier-Free Non-Linear Integer Arithmetic (QF_NIA).

## Syntax

### Variable Declaration

Declare variables with their types using the `var` keyword. Multiple variables of the same type can be declared together:

```
var p, q : bool
var m, n : int
```

### Types

SMTL supports two types:

- `bool` - Booleans (`T` for true, `F` for false)
- `int` - Integers

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

| Operator | Meaning        | Example |
| -------- | -------------- | ------- |
| `-`      | Negation       | `-m`    |
| `+`      | Addition       | `m + n` |
| `-`      | Subtraction    | `m - n` |
| `*`      | Multiplication | `m * n` |

#### Comparison Operators

| Operator | Meaning          | Example  |
| -------- | ---------------- | -------- |
| `=`      | Equality         | `m = n`  |
| `!=`     | Inequality       | `m != n` |
| `<`      | Less than        | `m < n`  |
| `>`      | Greater than     | `m > n`  |
| `<=`     | Less or equal    | `m <= n` |
| `>=`     | Greater or equal | `m >= n` |

### Variable Assignment

Use `let` to assign an expression to an identifier:

```
let f = (~p \/ q) /\ (p <=> q) /\ (F => T)
let g = -m * m + n - 1 < -1
```

### Assertions

Use `assert` to assert that an expression must be true:

```
assert f
assert g
```

## Implementation

SMTL consists of three main modules:

- **Parser** - Converts source code into an abstract syntax tree (AST)
- **Type Checker** - Validates type correctness and catches semantic errors
- **Solver** - Translates AST into SMT-LIB format

A separate Z3 process is spawned to solve the generated SMT-LIB problem.

### Example

We start with the following SMTL code:

```
var p, q : bool
var x, y : int

assert p \/ q
assert x > 0 /\ y > 0
assert x * y == 12
assert x < y
```

Parser converts it into an AST:

```
Declare [p, q] Bool
Declare [x, y] Int
Assert (Or p q)
Assert (And (Gt x 0) (Gt y 0))
Assert (Eq (Mul x y) 12)
Assert (Lt x y)
```

Type Checker looks for type errors and validates the program. If there are no errors, it passes the AST to the Solver, which generates the following SMT-LIB code:

```
(set-logic QF_NIA)
(declare-const p Bool)
(declare-const q Bool)
(declare-const x Int)
(declare-const y Int)
(assert (or p q))
(assert (and (> x 0) (> y 0)))
(assert (= (* x y) 12))
(assert (< x y))
(check-sat)
(get-model)
(exit)
```

Finally, Z3 is invoked with the generated SMT-LIB code, and it returns a solution that satisfies all the assertions:

```
sat
(
  (define-fun p () Bool
    true)
  (define-fun x () Int
    2)
  (define-fun y () Int
    6)
  (define-fun q () Bool
    false)
)
```

## Warning

This is not a production-ready tool. It is a personal project for learning purposes, and as such, it may contain bugs and is not optimized for performance.

## License

BSD-3-Clause © [grabczak](https://github.com/grabczak) 2026
