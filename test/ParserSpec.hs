module ParserSpec (tests) where

import Test.HUnit
import Text.Megaparsec (errorBundlePretty)

import AST
import Parser (parseProgram)

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Extract a statement from a program by index (0-based)
getStatement :: Program -> Int -> Maybe (Loc Statement)
getStatement (Program stmts) idx
  | idx >= 0 && idx < length stmts = Just (stmts !! idx)
  | otherwise = Nothing

-- Extract the expression from a Loc wrapper
getExpr :: Loc Expr -> Expr
getExpr (Loc _ _ e) = e

-- Extract the identifier from a Loc wrapper
getIdent :: Loc Identifier -> Identifier
getIdent (Loc _ _ i) = i

-- Extract operands from specific binary operators
getAddOperands :: Expr -> Maybe (Expr, Expr)
getAddOperands (Add e1 e2) = Just (getExpr e1, getExpr e2)
getAddOperands _ = Nothing

getSubOperands :: Expr -> Maybe (Expr, Expr)
getSubOperands (Sub e1 e2) = Just (getExpr e1, getExpr e2)
getSubOperands _ = Nothing

getMulOperands :: Expr -> Maybe (Expr, Expr)
getMulOperands (Mul e1 e2) = Just (getExpr e1, getExpr e2)
getMulOperands _ = Nothing

getNegOperand :: Expr -> Maybe Expr
getNegOperand (Neg e) = Just (getExpr e)
getNegOperand _ = Nothing

getAndOperands :: Expr -> Maybe (Expr, Expr)
getAndOperands (And e1 e2) = Just (getExpr e1, getExpr e2)
getAndOperands _ = Nothing

getOrOperands :: Expr -> Maybe (Expr, Expr)
getOrOperands (Or e1 e2) = Just (getExpr e1, getExpr e2)
getOrOperands _ = Nothing

getEqOperands :: Expr -> Maybe (Expr, Expr)
getEqOperands (Eq e1 e2) = Just (getExpr e1, getExpr e2)
getEqOperands _ = Nothing

getNeqOperands :: Expr -> Maybe (Expr, Expr)
getNeqOperands (Neq e1 e2) = Just (getExpr e1, getExpr e2)
getNeqOperands _ = Nothing

getLtOperands :: Expr -> Maybe (Expr, Expr)
getLtOperands (Lt e1 e2) = Just (getExpr e1, getExpr e2)
getLtOperands _ = Nothing

getGtOperands :: Expr -> Maybe (Expr, Expr)
getGtOperands (Gt e1 e2) = Just (getExpr e1, getExpr e2)
getGtOperands _ = Nothing

getLeqOperands :: Expr -> Maybe (Expr, Expr)
getLeqOperands (Leq e1 e2) = Just (getExpr e1, getExpr e2)
getLeqOperands _ = Nothing

getGeqOperands :: Expr -> Maybe (Expr, Expr)
getGeqOperands (Geq e1 e2) = Just (getExpr e1, getExpr e2)
getGeqOperands _ = Nothing

getImpliesOperands :: Expr -> Maybe (Expr, Expr)
getImpliesOperands (Implies e1 e2) = Just (getExpr e1, getExpr e2)
getImpliesOperands _ = Nothing

getIffOperands :: Expr -> Maybe (Expr, Expr)
getIffOperands (Iff e1 e2) = Just (getExpr e1, getExpr e2)
getIffOperands _ = Nothing

getNotOperand :: Expr -> Maybe Expr
getNotOperand (Not e) = Just (getExpr e)
getNotOperand _ = Nothing

-- Check expression equality by pattern matching
exprEq :: Expr -> Expr -> Bool
exprEq (BoolLit a) (BoolLit b) = a == b
exprEq (IntLit a) (IntLit b) = a == b
exprEq (Var a) (Var b) = a == b
exprEq _ _ = False

-- ============================================================================
-- Parser Tests
-- ============================================================================

-- Basic declarations
testParseSimpleDeclaration :: Test
testParseSimpleDeclaration = TestCase $ do
  let input = "var x : int"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Declare vars typ)) ->
        let names = map getIdent vars
            expectedNames = ["x"]
         in do
              assertEqual "Declared names" expectedNames names
              assertEqual "Declared type" Int typ
      _ -> assertFailure "Expected Declare statement"

testParseMultipleDeclarations :: Test
testParseMultipleDeclarations = TestCase $ do
  let input = "var x, y : int"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Declare vars typ)) ->
        let names = map getIdent vars
            expectedNames = ["x", "y"]
         in do
              assertEqual "Variable count" 2 (length names)
              assertEqual "Declared names" expectedNames names
              assertEqual "Declared type" Int typ
      _ -> assertFailure "Expected Declare statement"

testParseBoolDeclaration :: Test
testParseBoolDeclaration = TestCase $ do
  let input = "var flag : bool"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Declare vars typ)) ->
        let names = map getIdent vars
         in do
              assertEqual "Variable name" ["flag"] names
              assertEqual "Declared type" Bool typ
      _ -> assertFailure "Expected Declare statement"

testParseMixedTypes :: Test
testParseMixedTypes = TestCase $ do
  let input = "var x : int\nvar flag : bool"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> do
      case getStatement prog 0 of
        Just (Loc _ _ (Declare vars1 typ1)) -> do
          assertEqual "First declare type" Int typ1
          assertEqual "First declare names" ["x"] (map getIdent vars1)
        _ -> assertFailure "Expected first Declare statement"
      case getStatement prog 1 of
        Just (Loc _ _ (Declare vars2 typ2)) -> do
          assertEqual "Second declare type" Bool typ2
          assertEqual "Second declare names" ["flag"] (map getIdent vars2)
        _ -> assertFailure "Expected second Declare statement"

-- Simple assignments
testParseAssignLiteral :: Test
testParseAssignLiteral = TestCase $ do
  let input = "let x = 42"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign var expr)) -> do
        assertEqual "Variable name" "x" (getIdent var)
        assertBool "Expression equals IntLit 42" (exprEq (IntLit 42) (getExpr expr))
      _ -> assertFailure "Expected Assign statement"

testParseAssignBoolLiteral :: Test
testParseAssignBoolLiteral = TestCase $ do
  let input = "let flag = T"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign var expr)) -> do
        assertEqual "Variable name" "flag" (getIdent var)
        assertBool "Expression equals BoolLit True" (exprEq (BoolLit True) (getExpr expr))
      _ -> assertFailure "Expected Assign statement"

testParseAssignVariable :: Test
testParseAssignVariable = TestCase $ do
  let input = "let y = x"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign var expr)) -> do
        assertEqual "Variable name" "y" (getIdent var)
        assertBool "Expression equals Var x" (exprEq (Var "x") (getExpr expr))
      _ -> assertFailure "Expected Assign statement"

-- Arithmetic expressions
testParseAddition :: Test
testParseAddition = TestCase $ do
  let input = "let x = 2 + 3"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getAddOperands (getExpr expr) of
        Just (IntLit 2, IntLit 3) -> assertBool "Addition parsed correctly" True
        Just (l, r) -> assertFailure $ "Wrong operands: " ++ show l ++ " and " ++ show r
        Nothing -> assertFailure "Expected Add expression"
      _ -> assertFailure "Expected Assign statement"

testParseSubtraction :: Test
testParseSubtraction = TestCase $ do
  let input = "let x = 10 - 3"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getSubOperands (getExpr expr) of
        Just (IntLit 10, IntLit 3) -> assertBool "Subtraction parsed correctly" True
        _ -> assertFailure "Expected Sub expression with operands 10 and 3"
      _ -> assertFailure "Expected Assign statement"

testParseMultiplication :: Test
testParseMultiplication = TestCase $ do
  let input = "let x = 2 * 3"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getMulOperands (getExpr expr) of
        Just (IntLit 2, IntLit 3) -> assertBool "Multiplication parsed correctly" True
        _ -> assertFailure "Expected Mul expression with operands 2 and 3"
      _ -> assertFailure "Expected Assign statement"

testParseNegation :: Test
testParseNegation = TestCase $ do
  let input = "let x = -5"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getNegOperand (getExpr expr) of
        Just (IntLit 5) -> assertBool "Negation parsed correctly" True
        _ -> assertFailure "Expected Neg expression with operand 5"
      _ -> assertFailure "Expected Assign statement"

-- Operator precedence
testParsePrecedenceMultBeforeAdd :: Test
testParsePrecedenceMultBeforeAdd = TestCase $ do
  let input = "let x = 2 + 3 * 4"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getAddOperands (getExpr expr) of
        Just (IntLit 2, rhsExpr) -> case getMulOperands rhsExpr of
          Just (IntLit 3, IntLit 4) -> assertBool "Precedence correct (mult before add)" True
          _ -> assertFailure "Expected 3 * 4 on right side"
        _ -> assertFailure "Expected Add with 2 on left"
      _ -> assertFailure "Expected Assign statement"

testParsePrecedenceWithParens :: Test
testParsePrecedenceWithParens = TestCase $ do
  let input = "let x = (2 + 3) * 4"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getMulOperands (getExpr expr) of
        Just (lhsExpr, IntLit 4) -> case getAddOperands lhsExpr of
          Just (IntLit 2, IntLit 3) -> assertBool "Parentheses override precedence" True
          _ -> assertFailure "Expected 2 + 3 on left side"
        _ -> assertFailure "Expected Mul with 4 on right"
      _ -> assertFailure "Expected Assign statement"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getMulOperands (getExpr expr) of
        Just (lhsExpr, IntLit 4) -> case getAddOperands lhsExpr of
          Just (IntLit 2, IntLit 3) -> assertBool "Parentheses override precedence" True
          _ -> assertFailure "Expected 2 + 3 on left side"
        _ -> assertFailure "Expected Mul with 4 on right"
      _ -> assertFailure "Expected Assign statement"

-- Comparison operators
testParseEquality :: Test
testParseEquality = TestCase $ do
  let input = "let c = 5 == 5"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getEqOperands (getExpr expr) of
        Just (IntLit 5, IntLit 5) -> assertBool "Equality parsed correctly" True
        _ -> assertFailure "Expected Eq with operands 5 and 5"
      _ -> assertFailure "Expected Assign statement"

testParseInequality :: Test
testParseInequality = TestCase $ do
  let input = "let c = 3 != 4"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getNeqOperands (getExpr expr) of
        Just (IntLit 3, IntLit 4) -> assertBool "Inequality parsed correctly" True
        _ -> assertFailure "Expected Neq with operands 3 and 4"
      _ -> assertFailure "Expected Assign statement"

testParseLessThan :: Test
testParseLessThan = TestCase $ do
  let input = "let c = 2 < 5"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getLtOperands (getExpr expr) of
        Just (IntLit 2, IntLit 5) -> assertBool "Less than parsed correctly" True
        _ -> assertFailure "Expected Lt with operands 2 and 5"
      _ -> assertFailure "Expected Assign statement"

testParseGreaterThan :: Test
testParseGreaterThan = TestCase $ do
  let input = "let c = 10 > 3"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getGtOperands (getExpr expr) of
        Just (IntLit 10, IntLit 3) -> assertBool "Greater than parsed correctly" True
        _ -> assertFailure "Expected Gt with operands 10 and 3"
      _ -> assertFailure "Expected Assign statement"

testParseLessEqual :: Test
testParseLessEqual = TestCase $ do
  let input = "let c = 3 <= 8"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getLeqOperands (getExpr expr) of
        Just (IntLit 3, IntLit 8) -> assertBool "Less or equal parsed correctly" True
        _ -> assertFailure "Expected Leq with operands 3 and 8"
      _ -> assertFailure "Expected Assign statement"

testParseGreaterEqual :: Test
testParseGreaterEqual = TestCase $ do
  let input = "let c = 15 >= 5"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getGeqOperands (getExpr expr) of
        Just (IntLit 15, IntLit 5) -> assertBool "Greater or equal parsed correctly" True
        _ -> assertFailure "Expected Geq with operands 15 and 5"
      _ -> assertFailure "Expected Assign statement"

-- Logical operators
testParseAnd :: Test
testParseAnd = TestCase $ do
  let input = "let c = T /\\ F"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getAndOperands (getExpr expr) of
        Just (BoolLit True, BoolLit False) -> assertBool "And parsed correctly" True
        _ -> assertFailure "Expected And with operands T and F"
      _ -> assertFailure "Expected Assign statement"

testParseOr :: Test
testParseOr = TestCase $ do
  let input = "let c = T \\/ F"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getOrOperands (getExpr expr) of
        Just (BoolLit True, BoolLit False) -> assertBool "Or parsed correctly" True
        _ -> assertFailure "Expected Or with operands T and F"
      _ -> assertFailure "Expected Assign statement"

testParseNot :: Test
testParseNot = TestCase $ do
  let input = "let c = ~T"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getNotOperand (getExpr expr) of
        Just (BoolLit True) -> assertBool "Not parsed correctly" True
        _ -> assertFailure "Expected Not with operand T"
      _ -> assertFailure "Expected Assign statement"

testParseImplies :: Test
testParseImplies = TestCase $ do
  let input = "let c = T => F"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getImpliesOperands (getExpr expr) of
        Just (BoolLit True, BoolLit False) -> assertBool "Implies parsed correctly" True
        _ -> assertFailure "Expected Implies with operands T and F"
      _ -> assertFailure "Expected Assign statement"

testParseIff :: Test
testParseIff = TestCase $ do
  let input = "let c = T <=> F"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getIffOperands (getExpr expr) of
        Just (BoolLit True, BoolLit False) -> assertBool "Iff parsed correctly" True
        _ -> assertFailure "Expected Iff with operands T and F"
      _ -> assertFailure "Expected Assign statement"

-- Assertions
testParseSimpleAssert :: Test
testParseSimpleAssert = TestCase $ do
  let input = "assert T"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assert expr)) ->
        assertBool "Assert expression" (exprEq (BoolLit True) (getExpr expr))
      _ -> assertFailure "Expected Assert statement"

testParseAssertComparison :: Test
testParseAssertComparison = TestCase $ do
  let input = "assert 5 >= 0"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assert expr)) -> case getGeqOperands (getExpr expr) of
        Just (IntLit 5, IntLit 0) -> assertBool "Comparison assertion parsed" True
        _ -> assertFailure "Expected Geq in assert"
      _ -> assertFailure "Expected Assert statement"

testParseAssertLogical :: Test
testParseAssertLogical = TestCase $ do
  let input = "assert T /\\ F"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assert expr)) -> case getAndOperands (getExpr expr) of
        Just (BoolLit True, BoolLit False) -> assertBool "Logical assertion parsed" True
        _ -> assertFailure "Expected And in assert"
      _ -> assertFailure "Expected Assert statement"

-- Comments
testParseWithLineComment :: Test
testParseWithLineComment = TestCase $ do
  let input = "# This is a comment\nvar x : int"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Declare vars typ)) ->
        let names = map getIdent vars
         in do
              assertEqual "Variable name" ["x"] names
              assertEqual "Type" Int typ
      _ -> assertFailure "Expected Declare statement"

testParseWithCommentAfter :: Test
testParseWithCommentAfter = TestCase $ do
  let input = "var x : int # inline comment"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Declare vars typ)) ->
        let names = map getIdent vars
         in do
              assertEqual "Variable name" ["x"] names
              assertEqual "Type" Int typ
      _ -> assertFailure "Expected Declare statement"

-- Complex programs
testParseCompleteProgram :: Test
testParseCompleteProgram = TestCase $ do
  let input = "var x, y : int\nlet z = x + y\nassert z >= 0"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> do
      case getStatement prog 0 of
        Just (Loc _ _ (Declare _ _)) -> return ()
        _ -> assertFailure "Expected Declare statement"
      case getStatement prog 1 of
        Just (Loc _ _ (Assign _ _)) -> return ()
        _ -> assertFailure "Expected Assign statement"
      case getStatement prog 2 of
        Just (Loc _ _ (Assert _)) -> assertBool "Complete program parsed" True
        _ -> assertFailure "Expected Assert statement"

testParseProductionExample :: Test
testParseProductionExample = TestCase $ do
  let input = "var product_a, product_b : int\nlet labor = 2 * product_a + 3 * product_b\nassert labor <= 40"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> do
      case getStatement prog 0 of
        Just (Loc _ _ (Declare vars _)) ->
          let names = map getIdent vars
           in assertEqual "Declared names" ["product_a", "product_b"] names
        _ -> assertFailure "Expected Declare"
      case getStatement prog 1 of
        Just (Loc _ _ (Assign _ _)) -> assertBool "Production example parsed" True
        _ -> assertFailure "Expected Assign"

testParseEmpty :: Test
testParseEmpty = TestCase $ do
  let input = ""
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right (Program stmts) ->
      assertEqual "Empty program has no statements" 0 (length stmts)

testParseOnlyComments :: Test
testParseOnlyComments = TestCase $ do
  let input = "# just comments\n# more comments"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog ->
      case getStatement prog 0 of
        Nothing -> assertBool "Correctly parsed comment-only program as empty" True
        _ -> assertFailure "Should have empty program"

-- Variable names
testParseVarWithUnderscore :: Test
testParseVarWithUnderscore = TestCase $ do
  let input = "var labor_hours_used : int"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Declare vars _)) ->
        let names = map getIdent vars
         in assertEqual "Variable name with underscore" ["labor_hours_used"] names
      _ -> assertFailure "Expected Declare statement"

testParseVarWithNumbers :: Test
testParseVarWithNumbers = TestCase $ do
  let input = "var var123 : int"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Declare vars _)) ->
        let names = map getIdent vars
         in assertEqual "Variable name with numbers" ["var123"] names
      _ -> assertFailure "Expected Declare statement"

-- Error cases
testParseKeywordAsVar :: Test
testParseKeywordAsVar = TestCase $ do
  let input = "var var : int"
  case parseProgram "test" input of
    Right _ -> assertFailure "Should not parse 'var' as identifier"
    Left _ -> assertBool "Correctly rejected keyword as variable" True

testParseKeywordBool :: Test
testParseKeywordBool = TestCase $ do
  let input = "var bool : int"
  case parseProgram "test" input of
    Right _ -> assertFailure "Should not parse 'bool' as identifier"
    Left _ -> assertBool "Correctly rejected 'bool' as variable" True

-- Large numbers
testParseLargeNumber :: Test
testParseLargeNumber = TestCase $ do
  let input = "let x = 999999999"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) ->
        assertBool "Large number" (exprEq (IntLit 999999999) (getExpr expr))
      _ -> assertFailure "Expected Assign with large number"

-- Negative numbers
testParseNegativeNumber :: Test
testParseNegativeNumber = TestCase $ do
  let input = "let x = -42"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getNegOperand (getExpr expr) of
        Just (IntLit 42) -> assertBool "Negative number parsed" True
        _ -> assertFailure "Expected Neg with operand 42"
      _ -> assertFailure "Expected Assign"

testParseNestedNegation :: Test
testParseNestedNegation = TestCase $ do
  let input = "let x = -(-5)"
  case parseProgram "test" input of
    Left err -> assertFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right prog -> case getStatement prog 0 of
      Just (Loc _ _ (Assign _ expr)) -> case getNegOperand (getExpr expr) of
        Just negExpr -> case getNegOperand negExpr of
          Just (IntLit 5) -> assertBool "Nested negation parsed correctly" True
          _ -> assertFailure "Expected inner negation of 5"
        _ -> assertFailure "Expected outer negation"
      _ -> assertFailure "Expected Assign statement"

tests :: Test
tests =
  TestList
    [ TestLabel "Parse Simple Declaration" testParseSimpleDeclaration
    , TestLabel "Parse Multiple Declarations" testParseMultipleDeclarations
    , TestLabel "Parse Bool Declaration" testParseBoolDeclaration
    , TestLabel "Parse Mixed Types" testParseMixedTypes
    , TestLabel "Parse Assign Literal" testParseAssignLiteral
    , TestLabel "Parse Assign Bool Literal" testParseAssignBoolLiteral
    , TestLabel "Parse Assign Variable" testParseAssignVariable
    , TestLabel "Parse Addition" testParseAddition
    , TestLabel "Parse Subtraction" testParseSubtraction
    , TestLabel "Parse Multiplication" testParseMultiplication
    , TestLabel "Parse Negation" testParseNegation
    , TestLabel "Parse Precedence Mult Before Add" testParsePrecedenceMultBeforeAdd
    , TestLabel "Parse Precedence With Parens" testParsePrecedenceWithParens
    , TestLabel "Parse Equality" testParseEquality
    , TestLabel "Parse Inequality" testParseInequality
    , TestLabel "Parse Less Than" testParseLessThan
    , TestLabel "Parse Greater Than" testParseGreaterThan
    , TestLabel "Parse Less Equal" testParseLessEqual
    , TestLabel "Parse Greater Equal" testParseGreaterEqual
    , TestLabel "Parse And" testParseAnd
    , TestLabel "Parse Or" testParseOr
    , TestLabel "Parse Not" testParseNot
    , TestLabel "Parse Implies" testParseImplies
    , TestLabel "Parse Iff" testParseIff
    , TestLabel "Parse Simple Assert" testParseSimpleAssert
    , TestLabel "Parse Assert Comparison" testParseAssertComparison
    , TestLabel "Parse Assert Logical" testParseAssertLogical
    , TestLabel "Parse With Line Comment" testParseWithLineComment
    , TestLabel "Parse With Comment After" testParseWithCommentAfter
    , TestLabel "Parse Complete Program" testParseCompleteProgram
    , TestLabel "Parse Production Example" testParseProductionExample
    , TestLabel "Parse Empty" testParseEmpty
    , TestLabel "Parse Only Comments" testParseOnlyComments
    , TestLabel "Parse Var With Underscore" testParseVarWithUnderscore
    , TestLabel "Parse Var With Numbers" testParseVarWithNumbers
    , TestLabel "Parse Keyword As Var" testParseKeywordAsVar
    , TestLabel "Parse Keyword Bool" testParseKeywordBool
    , TestLabel "Parse Large Number" testParseLargeNumber
    , TestLabel "Parse Negative Number" testParseNegativeNumber
    , TestLabel "Parse Nested Negation" testParseNestedNegation
    ]
