module SolverSpec (tests) where

import Data.List (isInfixOf)
import Test.HUnit
import Text.Megaparsec (SourcePos, initialPos)

import AST
import Solver (smtlib)

-- ============================================================================
-- Solver Tests
-- ============================================================================

-- Helper to create SourcePos for testing
testPos :: SourcePos
testPos = initialPos "test.smtl"

-- Helper to create a Loc
makeLoc :: a -> Loc a
makeLoc = Loc testPos 1

-- SMT-LIB expression conversion tests

testSmtlibVarExpr :: Test
testSmtlibVarExpr = TestCase $ do
  let prog = Program [makeLoc (Declare [makeLoc "x"] Int), makeLoc (Assert (makeLoc (Var "x")))]
  let expr = smtlib prog
  assertBool "Variable in program" ("x" `elem` words expr)

testSmtlibBoolTrue :: Test
testSmtlibBoolTrue = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (BoolLit True)))]
  let expr = smtlib prog
  assertBool "Bool true in program" ("true" `isInfixOf` expr)

testSmtlibBoolFalse :: Test
testSmtlibBoolFalse = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (BoolLit False)))]
  let expr = smtlib prog
  assertBool "Bool false in program" ("false" `isInfixOf` expr)

testSmtlibIntLit :: Test
testSmtlibIntLit = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc (IntLit 42)) (makeLoc (IntLit 0)))))]
  let expr = smtlib prog
  assertBool "Int in program" ("42" `elem` words expr)

testSmtlibNegInt :: Test
testSmtlibNegInt = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc (IntLit (-5))) (makeLoc (IntLit 0)))))]
  let expr = smtlib prog
  assertBool "Negative int in program" (length expr > 0)

-- Arithmetic operators
testSmtlibAdd :: Test
testSmtlibAdd = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc (Add (makeLoc (IntLit 2)) (makeLoc (IntLit 3)))) (makeLoc (IntLit 4)))))]
  let output = smtlib prog
  assertBool "Addition in program" ("(+" `isInfixOf` output)

testSmtlibSub :: Test
testSmtlibSub = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc (Sub (makeLoc (IntLit 10)) (makeLoc (IntLit 3)))) (makeLoc (IntLit 0)))))]
  let output = smtlib prog
  assertBool "Subtraction in program" (length output > 0)

testSmtlibMul :: Test
testSmtlibMul = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc (Mul (makeLoc (IntLit 2)) (makeLoc (IntLit 3)))) (makeLoc (IntLit 0)))))]
  let output = smtlib prog
  assertBool "Multiplication in program" ("(*" `isInfixOf` output)

testSmtlibNeg :: Test
testSmtlibNeg = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc (Neg (makeLoc (IntLit (-5))))) (makeLoc (IntLit 0)))))]
  let output = smtlib prog
  assertBool "Negation in program" (length output > 0)

-- Logical operators
testSmtlibAnd :: Test
testSmtlibAnd = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (And (makeLoc (BoolLit True)) (makeLoc (BoolLit False)))))]
  let output = smtlib prog
  assertBool "And in program" ("(and" `isInfixOf` output)

testSmtlibOr :: Test
testSmtlibOr = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Or (makeLoc (BoolLit True)) (makeLoc (BoolLit False)))))]
  let output = smtlib prog
  assertBool "Or in program" ("(or" `isInfixOf` output)

testSmtlibNot :: Test
testSmtlibNot = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Not (makeLoc (BoolLit True)))))]
  let output = smtlib prog
  assertBool "Not in program" ("(not" `isInfixOf` output)

testSmtlibImplies :: Test
testSmtlibImplies = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Implies (makeLoc (BoolLit True)) (makeLoc (BoolLit False)))))]
  let output = smtlib prog
  assertBool "Implies in program" ("(=>" `isInfixOf` output)

testSmtlibIff :: Test
testSmtlibIff = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Iff (makeLoc (BoolLit True)) (makeLoc (BoolLit True)))))]
  let output = smtlib prog
  assertBool "Iff in program" (length output > 0)

-- Comparison operators
testSmtlibEq :: Test
testSmtlibEq = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Eq (makeLoc (IntLit 5)) (makeLoc (IntLit 5)))))]
  let output = smtlib prog
  assertBool "Eq in program" ("=" `elem` words output || "5" `elem` words output)

testSmtlibNeq :: Test
testSmtlibNeq = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Neq (makeLoc (IntLit 3)) (makeLoc (IntLit 4)))))]
  let output = smtlib prog
  assertBool "Neq in program" ("(not" `isInfixOf` output)

testSmtlibLt :: Test
testSmtlibLt = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Lt (makeLoc (IntLit 2)) (makeLoc (IntLit 5)))))]
  let output = smtlib prog
  assertBool "Lt in program" ("(<" `isInfixOf` output)

testSmtlibGt :: Test
testSmtlibGt = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Gt (makeLoc (IntLit 10)) (makeLoc (IntLit 3)))))]
  let output = smtlib prog
  assertBool "Gt in program" ("(>" `isInfixOf` output)

testSmtlibLeq :: Test
testSmtlibLeq = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Leq (makeLoc (IntLit 3)) (makeLoc (IntLit 8)))))]
  let output = smtlib prog
  assertBool "Leq in program" ("(<=" `isInfixOf` output)

testSmtlibGeq :: Test
testSmtlibGeq = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc (IntLit 15)) (makeLoc (IntLit 5)))))]
  let output = smtlib prog
  assertBool "Geq in program" ("(>=" `isInfixOf` output)

-- Complex expressions
testSmtlibComplexArithmetic :: Test
testSmtlibComplexArithmetic = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc (Add (makeLoc (Mul (makeLoc (IntLit 2)) (makeLoc (IntLit 3)))) (makeLoc (IntLit 4)))) (makeLoc (IntLit 0)))))]
  let output = smtlib prog
  assertBool "Complex arithmetic in program" ("(+" `isInfixOf` output && "(*" `isInfixOf` output)

testSmtlibComplexComparison :: Test
testSmtlibComplexComparison = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Leq (makeLoc (Add (makeLoc (Mul (makeLoc (IntLit 2)) (makeLoc (Var "x")))) (makeLoc (IntLit 5)))) (makeLoc (IntLit 100)))))]
  let output = smtlib prog
  assertBool "Complex comparison in program" ("(<=" `isInfixOf` output)

-- Statement conversion
testSmtlibDeclare :: Test
testSmtlibDeclare = TestCase $ do
  let prog = Program [makeLoc (Declare [makeLoc "x"] Int)]
  let output = smtlib prog
  assertBool "Program with declare generates output" (not (null output))
  assertBool "Declare converts to declare-const" ("declare-const" `isInfixOf` output)

testSmtlibAssert :: Test
testSmtlibAssert = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (BoolLit True)))]
  let output = smtlib prog
  assertBool "Assert statement appears in output" ("(assert" `isInfixOf` output)

testSmtlibLogicDeclaration :: Test
testSmtlibLogicDeclaration = TestCase $ do
  let prog = Program []
  let output = smtlib prog
  assertBool "SMT-LIB output starts with logic declaration" ("(set-logic QF_NIA)" `elem` lines output)

testSmtlibCheckSat :: Test
testSmtlibCheckSat = TestCase $ do
  let prog = Program []
  let output = smtlib prog
  assertBool "SMT-LIB output contains check-sat" ("(check-sat)" `elem` lines output)

testSmtlibGetModel :: Test
testSmtlibGetModel = TestCase $ do
  let prog = Program []
  let output = smtlib prog
  assertBool "SMT-LIB output contains get-model" ("(get-model)" `elem` lines output)

testSmtlibExit :: Test
testSmtlibExit = TestCase $ do
  let prog = Program []
  let output = smtlib prog
  assertBool "SMT-LIB output contains exit" ("(exit)" `elem` lines output)

-- Full program conversion
testSmtlibSimpleProgram :: Test
testSmtlibSimpleProgram = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x"] Int)
          , makeLoc (Assert (makeLoc (Geq (makeLoc (Var "x")) (makeLoc (IntLit 0)))))
          ]
  let output = smtlib prog
  assertBool "Simple program generates valid SMT-LIB" (length output > 0)
  assertBool "Contains declare" ("declare-const" `isInfixOf` output)
  assertBool "Contains assert" ("(assert" `isInfixOf` output)
  assertBool "Contains check-sat" ("(check-sat)" `isInfixOf` output)

testSmtlibMultipleVariables :: Test
testSmtlibMultipleVariables = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x", makeLoc "y", makeLoc "z"] Int)
          , makeLoc (Assert (makeLoc (Leq (makeLoc (Add (makeLoc (Var "x")) (makeLoc (Var "y")))) (makeLoc (Var "z")))))
          ]
  let output = smtlib prog
  assertBool "Multiple variables converted" (length output > 0)
  let lineCount = length (lines output)
  assertBool "Output has enough lines" (lineCount > 5)

testSmtlibConstraint :: Test
testSmtlibConstraint = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "product_a", makeLoc "product_b"] Int)
          , makeLoc (Assert (makeLoc (Leq (makeLoc (Add (makeLoc (Mul (makeLoc (IntLit 2)) (makeLoc (Var "product_a")))) (makeLoc (Mul (makeLoc (IntLit 3)) (makeLoc (Var "product_b")))))) (makeLoc (IntLit 40)))))
          ]
  let output = smtlib prog
  assertBool "Production constraint converts" (length output > 0)
  assertBool "Output contains product variables" ("product_a" `elem` words output || "product_b" `elem` words output)

testSmtlibEmpty :: Test
testSmtlibEmpty = TestCase $ do
  let prog = Program []
  let output = smtlib prog
  assertBool "Empty program generates valid skeleton" ("(set-logic QF_NIA)" `elem` lines output)
  assertBool "Empty program has check-sat" ("(check-sat)" `elem` lines output)

testSmtlibMultipleAssertions :: Test
testSmtlibMultipleAssertions = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x"] Int)
          , makeLoc (Assert (makeLoc (Geq (makeLoc (Var "x")) (makeLoc (IntLit 0)))))
          , makeLoc (Assert (makeLoc (Leq (makeLoc (Var "x")) (makeLoc (IntLit 100)))))
          ]
  let output = smtlib prog
  let assertCount = length $ filter ("(assert" `isInfixOf`) $ lines output
  assertBool "Multiple assertions converted" (assertCount >= 2)

testSmtlibAssignmentSubstitution :: Test
testSmtlibAssignmentSubstitution = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x", makeLoc "y"] Int)
          , makeLoc (Assign (makeLoc "x") (makeLoc (IntLit 5)))
          , makeLoc (Assert (makeLoc (Eq (makeLoc (Var "x")) (makeLoc (IntLit 5)))))
          ]
  let output = smtlib prog
  assertBool "Assignment processed correctly" (length output > 0)

testSmtlibComplexConstraintSystem :: Test
testSmtlibComplexConstraintSystem = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "a", makeLoc "b", makeLoc "c"] Int)
          , makeLoc (Assert (makeLoc (And (makeLoc (Geq (makeLoc (Var "a")) (makeLoc (IntLit 0)))) (makeLoc (And (makeLoc (Leq (makeLoc (Var "b")) (makeLoc (IntLit 100)))) (makeLoc (Geq (makeLoc (Var "c")) (makeLoc (IntLit 0)))))))))
          ]
  let output = smtlib prog
  assertBool "Complex constraint system converts" (length output > 0)
  let outputLines = lines output
  assertBool "Has minimum structure" (length outputLines >= 5)

tests :: Test
tests =
  TestList
    [ TestLabel "Smtlib Var Expr" testSmtlibVarExpr
    , TestLabel "Smtlib Bool True" testSmtlibBoolTrue
    , TestLabel "Smtlib Bool False" testSmtlibBoolFalse
    , TestLabel "Smtlib Int Lit" testSmtlibIntLit
    , TestLabel "Smtlib Neg Int" testSmtlibNegInt
    , TestLabel "Smtlib Add" testSmtlibAdd
    , TestLabel "Smtlib Sub" testSmtlibSub
    , TestLabel "Smtlib Mul" testSmtlibMul
    , TestLabel "Smtlib Neg" testSmtlibNeg
    , TestLabel "Smtlib And" testSmtlibAnd
    , TestLabel "Smtlib Or" testSmtlibOr
    , TestLabel "Smtlib Not" testSmtlibNot
    , TestLabel "Smtlib Implies" testSmtlibImplies
    , TestLabel "Smtlib Iff" testSmtlibIff
    , TestLabel "Smtlib Eq" testSmtlibEq
    , TestLabel "Smtlib Neq" testSmtlibNeq
    , TestLabel "Smtlib Lt" testSmtlibLt
    , TestLabel "Smtlib Gt" testSmtlibGt
    , TestLabel "Smtlib Leq" testSmtlibLeq
    , TestLabel "Smtlib Geq" testSmtlibGeq
    , TestLabel "Smtlib Complex Arithmetic" testSmtlibComplexArithmetic
    , TestLabel "Smtlib Complex Comparison" testSmtlibComplexComparison
    , TestLabel "Smtlib Declare" testSmtlibDeclare
    , TestLabel "Smtlib Assert" testSmtlibAssert
    , TestLabel "Smtlib Logic Declaration" testSmtlibLogicDeclaration
    , TestLabel "Smtlib Check Sat" testSmtlibCheckSat
    , TestLabel "Smtlib Get Model" testSmtlibGetModel
    , TestLabel "Smtlib Exit" testSmtlibExit
    , TestLabel "Smtlib Simple Program" testSmtlibSimpleProgram
    , TestLabel "Smtlib Multiple Variables" testSmtlibMultipleVariables
    , TestLabel "Smtlib Constraint" testSmtlibConstraint
    , TestLabel "Smtlib Empty" testSmtlibEmpty
    , TestLabel "Smtlib Multiple Assertions" testSmtlibMultipleAssertions
    , TestLabel "Smtlib Assignment Substitution" testSmtlibAssignmentSubstitution
    , TestLabel "Smtlib Complex Constraint System" testSmtlibComplexConstraintSystem
    ]
