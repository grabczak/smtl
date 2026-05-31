module TypeCheckerSpec (tests) where

import Test.HUnit
import Text.Megaparsec (SourcePos, initialPos)

import AST
import TypeChecker (checkProgram)

-- ============================================================================
-- TypeChecker Tests
-- ============================================================================

-- Helper to create SourcePos for testing
testPos :: SourcePos
testPos = initialPos "test.smtl"

-- Helper to create a Loc
makeLoc :: a -> Loc a
makeLoc = Loc testPos 1

-- Valid programs
testCheckSimpleDeclaration :: Test
testCheckSimpleDeclaration = TestCase $ do
  let prog = Program [makeLoc (Declare [makeLoc "x"] Int)]
  case checkProgram prog of
    Left _ -> assertFailure "Valid declaration failed type check"
    Right _ -> assertBool "Valid declaration passed" True

testCheckBoolDeclaration :: Test
testCheckBoolDeclaration = TestCase $ do
  let prog = Program [makeLoc (Declare [makeLoc "flag"] Bool)]
  case checkProgram prog of
    Left _ -> assertFailure "Valid bool declaration failed"
    Right _ -> assertBool "Valid bool declaration passed" True

testCheckMultipleDeclarations :: Test
testCheckMultipleDeclarations = TestCase $ do
  let prog = Program [makeLoc (Declare [makeLoc "x", makeLoc "y"] Int)]
  case checkProgram prog of
    Left _ -> assertFailure "Valid multiple declarations failed"
    Right _ -> assertBool "Valid multiple declarations passed" True

testCheckAssignAfterDeclare :: Test
testCheckAssignAfterDeclare = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x"] Int)
          , makeLoc (Assign (makeLoc "y") (makeLoc (IntLit 42)))
          ]
  case checkProgram prog of
    Left _ -> assertFailure "Valid assignment after declare failed"
    Right _ -> assertBool "Valid assignment after declare passed" True

testCheckAssignExpression :: Test
testCheckAssignExpression = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x", makeLoc "y"] Int)
          , makeLoc (Assign (makeLoc "z") (makeLoc (Add (makeLoc (Var "x")) (makeLoc (Var "y")))))
          ]
  case checkProgram prog of
    Left _ -> assertFailure "Valid expression assignment failed"
    Right _ -> assertBool "Valid expression assignment passed" True

testCheckSimpleAssertion :: Test
testCheckSimpleAssertion = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (BoolLit True)))]
  case checkProgram prog of
    Left _ -> assertFailure "Valid simple assertion failed"
    Right _ -> assertBool "Valid simple assertion passed" True

testCheckAssertComparison :: Test
testCheckAssertComparison = TestCase $ do
  let expr = Geq (makeLoc (IntLit 10)) (makeLoc (IntLit 5))
  let prog = Program [makeLoc (Assert (makeLoc expr))]
  case checkProgram prog of
    Left _ -> assertFailure "Valid comparison assertion failed"
    Right _ -> assertBool "Valid comparison assertion passed" True

testCheckAssertLogical :: Test
testCheckAssertLogical = TestCase $ do
  let expr = And (makeLoc (BoolLit True)) (makeLoc (BoolLit True))
  let prog = Program [makeLoc (Assert (makeLoc expr))]
  case checkProgram prog of
    Left _ -> assertFailure "Valid logical assertion failed"
    Right _ -> assertBool "Valid logical assertion passed" True

testCheckCompleteProgram :: Test
testCheckCompleteProgram = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x", makeLoc "y"] Int)
          , makeLoc (Assign (makeLoc "z") (makeLoc (Add (makeLoc (Var "x")) (makeLoc (Var "y")))))
          , makeLoc (Assert (makeLoc (Geq (makeLoc (Var "z")) (makeLoc (IntLit 0)))))
          ]
  case checkProgram prog of
    Left _ -> assertFailure "Valid complete program failed"
    Right _ -> assertBool "Valid complete program passed" True

testCheckArithmeticConstraint :: Test
testCheckArithmeticConstraint = TestCase $ do
  let constraint = Leq (makeLoc (Add (makeLoc (Mul (makeLoc (IntLit 2)) (makeLoc (Var "x")))) (makeLoc (IntLit 5)))) (makeLoc (IntLit 100))
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x"] Int)
          , makeLoc (Assert (makeLoc constraint))
          ]
  case checkProgram prog of
    Left _ -> assertFailure "Valid arithmetic constraint failed"
    Right _ -> assertBool "Valid arithmetic constraint passed" True

-- Error: unbound variable
testCheckUnboundVariable :: Test
testCheckUnboundVariable = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (Var "undefined")))]
  case checkProgram prog of
    Left (Loc _ _ (UnboundVariable "undefined")) -> assertBool "Caught unbound variable" True
    Left _ -> assertFailure "Wrong error type for unbound variable"
    Right _ -> assertFailure "Should have caught unbound variable"

testCheckUnboundInExpression :: Test
testCheckUnboundInExpression = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x"] Int)
          , makeLoc (Assign (makeLoc "y") (makeLoc (Add (makeLoc (Var "x")) (makeLoc (Var "undefined")))))
          ]
  case checkProgram prog of
    Left (Loc _ _ (UnboundVariable "undefined")) -> assertBool "Caught unbound variable in expression" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught unbound variable"

-- Error: duplicate identifier
testCheckDuplicateIdentifier :: Test
testCheckDuplicateIdentifier = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x"] Int)
          , makeLoc (Declare [makeLoc "x"] Bool)
          ]
  case checkProgram prog of
    Left (Loc _ _ (DuplicateIdentifier "x")) -> assertBool "Caught duplicate identifier" True
    Left _ -> assertFailure "Wrong error type for duplicate"
    Right _ -> assertFailure "Should have caught duplicate identifier"

testCheckDuplicateInAssign :: Test
testCheckDuplicateInAssign = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x"] Int)
          , makeLoc (Assign (makeLoc "x") (makeLoc (IntLit 5)))
          ]
  case checkProgram prog of
    Left (Loc _ _ (DuplicateIdentifier "x")) -> assertBool "Caught duplicate in assign" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught duplicate in assignment"

-- Error: type mismatch in assertions
testCheckAssertIntExpression :: Test
testCheckAssertIntExpression = TestCase $ do
  let prog = Program [makeLoc (Assert (makeLoc (IntLit 5)))]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Bool Int)) -> assertBool "Caught type mismatch in assertion" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

testCheckAssertArithmetic :: Test
testCheckAssertArithmetic = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x", makeLoc "y"] Int)
          , makeLoc (Assert (makeLoc (Add (makeLoc (Var "x")) (makeLoc (Var "y")))))
          ]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Bool Int)) -> assertBool "Caught arithmetic as bool" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

-- Error: type mismatch in logical operations
testCheckAndWithInts :: Test
testCheckAndWithInts = TestCase $ do
  let expr = And (makeLoc (IntLit 1)) (makeLoc (BoolLit True))
  let prog = Program [makeLoc (Assert (makeLoc expr))]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Bool Int)) -> assertBool "Caught int in AND" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

testCheckOrWithInts :: Test
testCheckOrWithInts = TestCase $ do
  let expr = Or (makeLoc (BoolLit True)) (makeLoc (IntLit 5))
  let prog = Program [makeLoc (Assert (makeLoc expr))]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Bool Int)) -> assertBool "Caught int in OR" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

testCheckNotWithInt :: Test
testCheckNotWithInt = TestCase $ do
  let expr = Not (makeLoc (IntLit 5))
  let prog = Program [makeLoc (Assert (makeLoc expr))]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Bool Int)) -> assertBool "Caught int in NOT" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

-- Error: type mismatch in arithmetic operations
testCheckAddBools :: Test
testCheckAddBools = TestCase $ do
  let expr = Add (makeLoc (BoolLit True)) (makeLoc (BoolLit False))
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc expr) (makeLoc (IntLit 0)))))]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Int Bool)) -> assertBool "Caught bool in ADD" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

testCheckSubBools :: Test
testCheckSubBools = TestCase $ do
  let expr = Sub (makeLoc (BoolLit True)) (makeLoc (BoolLit False))
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc expr) (makeLoc (IntLit 0)))))]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Int Bool)) -> assertBool "Caught bool in SUB" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

testCheckMulBools :: Test
testCheckMulBools = TestCase $ do
  let expr = Mul (makeLoc (BoolLit True)) (makeLoc (BoolLit False))
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc expr) (makeLoc (IntLit 0)))))]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Int Bool)) -> assertBool "Caught bool in MUL" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

testCheckNegBool :: Test
testCheckNegBool = TestCase $ do
  let expr = Neg (makeLoc (BoolLit True))
  let prog = Program [makeLoc (Assert (makeLoc (Geq (makeLoc expr) (makeLoc (IntLit 0)))))]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Int Bool)) -> assertBool "Caught bool in NEG" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

-- Error: type mismatch in comparisons (comparisons require ints)
testCheckEqMismatch :: Test
testCheckEqMismatch = TestCase $ do
  let expr = Eq (makeLoc (BoolLit True)) (makeLoc (IntLit 1))
  let prog = Program [makeLoc (Assert (makeLoc expr))]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Int Bool)) -> assertBool "Caught type mismatch in EQ" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

testCheckLtMismatch :: Test
testCheckLtMismatch = TestCase $ do
  let expr = Lt (makeLoc (BoolLit True)) (makeLoc (IntLit 5))
  let prog = Program [makeLoc (Assert (makeLoc expr))]
  case checkProgram prog of
    Left (Loc _ _ (TypeMismatch Int Bool)) -> assertBool "Caught type mismatch in LT" True
    Left _ -> assertFailure "Wrong error type"
    Right _ -> assertFailure "Should have caught type mismatch"

-- Complex valid scenarios
testCheckProductionExample :: Test
testCheckProductionExample = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "product_a", makeLoc "product_b", makeLoc "product_c"] Int)
          , makeLoc (Assign (makeLoc "total_profit") (makeLoc (Add (makeLoc (Add (makeLoc (Mul (makeLoc (IntLit 15)) (makeLoc (Var "product_a")))) (makeLoc (Mul (makeLoc (IntLit 17)) (makeLoc (Var "product_b")))))) (makeLoc (Mul (makeLoc (IntLit 9)) (makeLoc (Var "product_c")))))))
          , makeLoc (Assert (makeLoc (Geq (makeLoc (Var "total_profit")) (makeLoc (IntLit 100)))))
          ]
  case checkProgram prog of
    Left _ -> assertFailure "Valid production example failed type check"
    Right _ -> assertBool "Valid production example passed" True

testCheckConstraintChaining :: Test
testCheckConstraintChaining = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x"] Int)
          , makeLoc (Assert (makeLoc (And (makeLoc (Geq (makeLoc (Var "x")) (makeLoc (IntLit 0)))) (makeLoc (Leq (makeLoc (Var "x")) (makeLoc (IntLit 100)))))))
          ]
  case checkProgram prog of
    Left _ -> assertFailure "Valid constraint chaining failed"
    Right _ -> assertBool "Valid constraint chaining passed" True

testCheckMultipleConstraints :: Test
testCheckMultipleConstraints = TestCase $ do
  let prog =
        Program
          [ makeLoc (Declare [makeLoc "x", makeLoc "y"] Int)
          , makeLoc (Assert (makeLoc (Geq (makeLoc (Var "x")) (makeLoc (IntLit 0)))))
          , makeLoc (Assert (makeLoc (Geq (makeLoc (Var "y")) (makeLoc (IntLit 0)))))
          , makeLoc (Assert (makeLoc (Leq (makeLoc (Add (makeLoc (Var "x")) (makeLoc (Var "y")))) (makeLoc (IntLit 100)))))
          ]
  case checkProgram prog of
    Left _ -> assertFailure "Valid multiple constraints failed"
    Right _ -> assertBool "Valid multiple constraints passed" True

tests :: Test
tests =
  TestList
    [ TestLabel "Check Simple Declaration" testCheckSimpleDeclaration
    , TestLabel "Check Bool Declaration" testCheckBoolDeclaration
    , TestLabel "Check Multiple Declarations" testCheckMultipleDeclarations
    , TestLabel "Check Assign After Declare" testCheckAssignAfterDeclare
    , TestLabel "Check Assign Expression" testCheckAssignExpression
    , TestLabel "Check Simple Assertion" testCheckSimpleAssertion
    , TestLabel "Check Assert Comparison" testCheckAssertComparison
    , TestLabel "Check Assert Logical" testCheckAssertLogical
    , TestLabel "Check Complete Program" testCheckCompleteProgram
    , TestLabel "Check Arithmetic Constraint" testCheckArithmeticConstraint
    , TestLabel "Check Unbound Variable" testCheckUnboundVariable
    , TestLabel "Check Unbound In Expression" testCheckUnboundInExpression
    , TestLabel "Check Duplicate Identifier" testCheckDuplicateIdentifier
    , TestLabel "Check Duplicate In Assign" testCheckDuplicateInAssign
    , TestLabel "Check Assert Int Expression" testCheckAssertIntExpression
    , TestLabel "Check Assert Arithmetic" testCheckAssertArithmetic
    , TestLabel "Check And With Ints" testCheckAndWithInts
    , TestLabel "Check Or With Ints" testCheckOrWithInts
    , TestLabel "Check Not With Int" testCheckNotWithInt
    , TestLabel "Check Add Bools" testCheckAddBools
    , TestLabel "Check Sub Bools" testCheckSubBools
    , TestLabel "Check Mul Bools" testCheckMulBools
    , TestLabel "Check Neg Bool" testCheckNegBool
    , TestLabel "Check Eq Mismatch" testCheckEqMismatch
    , TestLabel "Check Lt Mismatch" testCheckLtMismatch
    , TestLabel "Check Production Example" testCheckProductionExample
    , TestLabel "Check Constraint Chaining" testCheckConstraintChaining
    , TestLabel "Check Multiple Constraints" testCheckMultipleConstraints
    ]
