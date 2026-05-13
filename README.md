# SMTL

My custom language for writing satisfiability modulo theories (SMT) problems.

## Setup

Install all the necessary Haskell tools via [GHCup](https://www.haskell.org/ghcup/).

You can verify the installation by checking the versions:

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

Basic syntax:

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

This program declares four variables and defines two constraints. When solved, the SMT solver will find assignments to `p`, `q`, `m`, and `n` that satisfy both `f` and `g`, or determine that the constraints are unsatisfiable.

## Language Overview

Essentially, you can think of an SMT formula as a SAT formula with additional types, such as integers, reals, or even strings and arrays.

SMTL is a language that allows you to write SMT problems in a convenient format, which is then translated into SMT-LIB for solving.

As of today, SMTL supports booleans and integers.
All computations are performed in the theory of Quantifier-Free Nonlinear Integer Arithmetic (QF_NIA).

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
let f = (~p \/ q) /\ (p <=> q)
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

## Warning

This is not a production-ready tool. It is a personal project for learning purposes, and as such, it may contain bugs and is not optimized for performance.

## License

BSD-3-Clause © [grabczak](https://github.com/grabczak) 2026
