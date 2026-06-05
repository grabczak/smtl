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

# Logic (QF_LIA or QF_NIA)
set logic QF_NIA

# Declarations
var p, q : bool
var m, n : int

# Assignments
let f = (p \/ ~p) /\ (F => T)
let g = (q /\ ~q) \/ (p <=> q)
let h = m * m + 1 < -5 * n

# Assertions
assert f /\ g
assert h
assert (m > 0 /\ n >= 0) \/ (m < 0 /\ n <= 0)
assert (m == n) /\ n != -2

# Output commands
check sat
get model
exit
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
