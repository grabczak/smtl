{-# LANGUAGE GADTs #-}

import Parser
import Test.HUnit
import Text.Megaparsec (parse)

-- ============================================================================
-- TYPE PARSING TESTS
-- ============================================================================

typeTestsBool :: Test
typeTestsBool = TestCase $ do
  let input = "var x : bool"
  case parse Parser.program "" input of
    Right (Program [Var "x" Bool]) -> return ()
    _ -> assertFailure "Failed to parse bool type"

typesTestsInt :: Test
typesTestsInt = TestCase $ do
  let input = "var x : int"
  case parse Parser.program "" input of
    Right (Program [Var "x" Int]) -> return ()
    _ -> assertFailure "Failed to parse int type"

-- ============================================================================
-- VARIABLE DECLARATION TESTS
-- ============================================================================

varDeclSimple :: Test
varDeclSimple = TestCase $ do
  let input = "var foo : int"
  case parse Parser.program "" input of
    Right (Program [Var "foo" Int]) -> return ()
    _ -> assertFailure "Failed to parse simple var declaration"

varDeclMultiChar :: Test
varDeclMultiChar = TestCase $ do
  let input = "var my_variable : int"
  case parse Parser.program "" input of
    Right (Program [Var "my_variable" Int]) -> return ()
    _ -> assertFailure "Failed to parse multi-char var with underscore"

varDeclMultiple :: Test
varDeclMultiple = TestCase $ do
  let input = "var x : int\nvar y : bool"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 2 declarations" 2 (length stmts)
    _ -> assertFailure "Failed to parse multiple var declarations"

varDeclWithWhitespace :: Test
varDeclWithWhitespace = TestCase $ do
  let input = "  var   x   :   int  "
  case parse Parser.program "" input of
    Right (Program [Var "x" Int]) -> return ()
    _ -> assertFailure "Failed to parse var with extra whitespace"

-- ============================================================================
-- BOOLEAN LITERAL TESTS
-- ============================================================================

boolLitTrue :: Test
boolLitTrue = TestCase $ do
  let input = "let x = true"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse boolean true literal"

boolLitFalse :: Test
boolLitFalse = TestCase $ do
  let input = "let x = false"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse boolean false literal"

-- ============================================================================
-- INTEGER LITERAL TESTS
-- ============================================================================

intLitZero :: Test
intLitZero = TestCase $ do
  let input = "let x = 0"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse integer literal zero"

intLitPositive :: Test
intLitPositive = TestCase $ do
  let input = "let x = 42"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse positive integer literal"

intLitLarge :: Test
intLitLarge = TestCase $ do
  let input = "let x = 999999999"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse large integer literal"

intLitNegative :: Test
intLitNegative = TestCase $ do
  let input = "let x = -42"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse negative integer literal"

intLitNegativeZero :: Test
intLitNegativeZero = TestCase $ do
  let input = "let x = -0"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse negative zero"

-- ============================================================================
-- UNARY OPERATOR TESTS
-- ============================================================================

unaryNotBool :: Test
unaryNotBool = TestCase $ do
  let input = "let x = ~ true"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse unary boolean not"

unaryNotIdentifier :: Test
unaryNotIdentifier = TestCase $ do
  let input = "let x = ~ y"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse unary not on identifier"

unaryNotNested :: Test
unaryNotNested = TestCase $ do
  let input = "let x = ~ ~ true"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse nested unary not"

unaryNegateInt :: Test
unaryNegateInt = TestCase $ do
  let input = "let x = - 42"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse unary negate on int"

unaryNegateIdentifier :: Test
unaryNegateIdentifier = TestCase $ do
  let input = "let x = - y"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse unary negate on identifier"

unaryNegateNested :: Test
unaryNegateNested = TestCase $ do
  let input = "let x = - - 5"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse nested unary negate"

-- ============================================================================
-- BINARY AND OPERATOR TESTS
-- ============================================================================

binaryAndBasic :: Test
binaryAndBasic = TestCase $ do
  let input = "let x = true /\\ false"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse binary and"

binaryAndChained :: Test
binaryAndChained = TestCase $ do
  let input = "let x = true /\\ false /\\ true"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse chained and"

binaryAndWithIdentifiers :: Test
binaryAndWithIdentifiers = TestCase $ do
  let input = "let x = a /\\ b"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse and with identifiers"

-- ============================================================================
-- BINARY OR OPERATOR TESTS
-- ============================================================================

binaryOrBasic :: Test
binaryOrBasic = TestCase $ do
  let input = "let x = true \\/ false"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse binary or"

binaryOrChained :: Test
binaryOrChained = TestCase $ do
  let input = "let x = true \\/ false \\/ true"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse chained or"

binaryOrWithIdentifiers :: Test
binaryOrWithIdentifiers = TestCase $ do
  let input = "let x = a \\/ b"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse or with identifiers"

-- ============================================================================
-- BINARY IMPLIES OPERATOR TESTS
-- ============================================================================

binaryImpliesBasic :: Test
binaryImpliesBasic = TestCase $ do
  let input = "let x = true => false"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse implies operator"

binaryImpliesChained :: Test
binaryImpliesChained = TestCase $ do
  let input = "let x = true => false => true"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse chained implies"

-- ============================================================================
-- BINARY IFF OPERATOR TESTS
-- ============================================================================

binaryIffBasic :: Test
binaryIffBasic = TestCase $ do
  let input = "let x = true <=> false"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse iff operator"

binaryIffChained :: Test
binaryIffChained = TestCase $ do
  let input = "let x = true <=> false <=> true"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse chained iff"

-- ============================================================================
-- EQUALITY OPERATOR TESTS
-- ============================================================================

equalityBool :: Test
equalityBool = TestCase $ do
  let input = "let x = true = false"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse equality on bools"

equalityInt :: Test
equalityInt = TestCase $ do
  let input = "let x = 5 = 10"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse equality on ints"

equalityChained :: Test
equalityChained = TestCase $ do
  let input = "let x = a = b = c"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse chained equality"

-- ============================================================================
-- INEQUALITY OPERATOR TESTS
-- ============================================================================

inequalityBool :: Test
inequalityBool = TestCase $ do
  let input = "let x = true != false"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse inequality on bools"

inequalityInt :: Test
inequalityInt = TestCase $ do
  let input = "let x = 5 != 10"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse inequality on ints"

-- ============================================================================
-- RELATIONAL OPERATOR TESTS
-- ============================================================================

lessThanInt :: Test
lessThanInt = TestCase $ do
  let input = "let x = 5 < 10"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse less than"

greaterThanInt :: Test
greaterThanInt = TestCase $ do
  let input = "let x = 10 > 5"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse greater than"

lessThanOrEqualInt :: Test
lessThanOrEqualInt = TestCase $ do
  let input = "let x = 5 <= 10"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse less than or equal"

greaterThanOrEqualInt :: Test
greaterThanOrEqualInt = TestCase $ do
  let input = "let x = 10 >= 5"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse greater than or equal"

-- ============================================================================
-- ARITHMETIC OPERATOR TESTS - ADDITION
-- ============================================================================

addIntBasic :: Test
addIntBasic = TestCase $ do
  let input = "let x = 5 + 10"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse integer addition"

addIntChained :: Test
addIntChained = TestCase $ do
  let input = "let x = 1 + 2 + 3 + 4"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse chained integer addition"

addIntNegative :: Test
addIntNegative = TestCase $ do
  let input = "let x = 10 + (-5)"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse integer addition with negative"

-- ============================================================================
-- ARITHMETIC OPERATOR TESTS - SUBTRACTION
-- ============================================================================

subIntBasic :: Test
subIntBasic = TestCase $ do
  let input = "let x = 10 - 5"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse integer subtraction"

subIntChained :: Test
subIntChained = TestCase $ do
  let input = "let x = 10 - 5 - 2"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse chained integer subtraction"

-- ============================================================================
-- ARITHMETIC OPERATOR TESTS - MULTIPLICATION
-- ============================================================================

mulIntBasic :: Test
mulIntBasic = TestCase $ do
  let input = "let x = 5 * 10"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse integer multiplication"

mulIntChained :: Test
mulIntChained = TestCase $ do
  let input = "let x = 2 * 3 * 4"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse chained integer multiplication"

-- ============================================================================
-- ARITHMETIC OPERATOR TESTS - DIVISION
-- ============================================================================

divIntBasic :: Test
divIntBasic = TestCase $ do
  let input = "let x = 10 / 5"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse integer division"

-- ============================================================================
-- OPERATOR PRECEDENCE TESTS
-- ============================================================================

precedenceAddVsMul :: Test
precedenceAddVsMul = TestCase $ do
  let input = "let x = 2 + 3 * 4"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse precedence: addition vs multiplication"

precedenceMulVsAdd :: Test
precedenceMulVsAdd = TestCase $ do
  let input = "let x = 2 * 3 + 4"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse precedence: multiplication vs addition"

precedenceAddVsSub :: Test
precedenceAddVsSub = TestCase $ do
  let input = "let x = 10 - 3 + 2"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse precedence: subtraction vs addition"

precedenceComparisonVsArithmetic :: Test
precedenceComparisonVsArithmetic = TestCase $ do
  let input = "let x = 2 + 3 < 10"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse precedence: comparison vs arithmetic"

precedenceAndVsOr :: Test
precedenceAndVsOr = TestCase $ do
  let input = "let x = true /\\ false \\/ true"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse precedence: and vs or"

precedenceOrVsImplies :: Test
precedenceOrVsImplies = TestCase $ do
  let input = "let x = true \\/ false => true"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse precedence: or vs implies"

precedenceComparisonVsLogical :: Test
precedenceComparisonVsLogical = TestCase $ do
  let input = "let x = 5 < 10 /\\ 20 > 15"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse precedence: comparison vs logical"

precedenceUnaryVsBinary :: Test
precedenceUnaryVsBinary = TestCase $ do
  let input = "let x = - 5 + 3"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse precedence: unary vs binary"

-- ============================================================================
-- PARENTHESES TESTS
-- ============================================================================

parenthesesSimple :: Test
parenthesesSimple = TestCase $ do
  let input = "let x = (5)"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse simple parentheses"

parenthesesBinaryOp :: Test
parenthesesBinaryOp = TestCase $ do
  let input = "let x = (5 + 3)"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse parentheses with binary op"

parenthesesOverridePrecedence :: Test
parenthesesOverridePrecedence = TestCase $ do
  let input = "let x = (2 + 3) * 4"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse parentheses overriding precedence"

parenthesesNested :: Test
parenthesesNested = TestCase $ do
  let input = "let x = ((2 + 3) * (4 + 5))"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse nested parentheses"

parenthesesDeeplyNested :: Test
parenthesesDeeplyNested = TestCase $ do
  let input = "let x = (((((5)))))"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse deeply nested parentheses"

parenthesesUnary :: Test
parenthesesUnary = TestCase $ do
  let input = "let x = (- 5)"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse parentheses with unary op"

parenthesesLogical :: Test
parenthesesLogical = TestCase $ do
  let input = "let x = (true /\\ false) \\/ (true /\\ false)"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse parentheses with logical ops"

-- ============================================================================
-- IDENTIFIER TESTS
-- ============================================================================

identifierSingleChar :: Test
identifierSingleChar = TestCase $ do
  let input = "let x = a"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse single character identifier"

identifierMultiChar :: Test
identifierMultiChar = TestCase $ do
  let input = "let x = myVariable"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse multi-character identifier"

identifierWithUnderscore :: Test
identifierWithUnderscore = TestCase $ do
  let input = "let x = my_var_name"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse identifier with underscores"

identifierWithNumbers :: Test
identifierWithNumbers = TestCase $ do
  let input = "let x = var123"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse identifier with numbers"

identifierStartsWithLetter :: Test
identifierStartsWithLetter = TestCase $ do
  let input = "let x = z_123_abc"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse identifier starting with letter"

identifierInExpression :: Test
identifierInExpression = TestCase $ do
  let input = "let x = a + b"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse identifiers in expression"

identifierInComparison :: Test
identifierInComparison = TestCase $ do
  let input = "let x = a < b"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse identifiers in comparison"

-- ============================================================================
-- LET DECLARATION TESTS
-- ============================================================================

letDeclBasic :: Test
letDeclBasic = TestCase $ do
  let input = "let x = 5"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse basic let declaration"

letDeclBool :: Test
letDeclBool = TestCase $ do
  let input = "let flag = true"
  case parse Parser.program "" input of
    Right (Program [Let "flag" _]) -> return ()
    _ -> assertFailure "Failed to parse let with boolean"

letDeclComplex :: Test
letDeclComplex = TestCase $ do
  let input = "let result = 2 + 3 * 4"
  case parse Parser.program "" input of
    Right (Program [Let "result" _]) -> return ()
    _ -> assertFailure "Failed to parse let with complex expression"

letDeclLogical :: Test
letDeclLogical = TestCase $ do
  let input = "let cond = true /\\ false"
  case parse Parser.program "" input of
    Right (Program [Let "cond" _]) -> return ()
    _ -> assertFailure "Failed to parse let with logical expression"

letDeclMultiple :: Test
letDeclMultiple = TestCase $ do
  let input = "let x = 5\nlet y = 10\nlet z = x + y"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 3 let declarations" 3 (length stmts)
    _ -> assertFailure "Failed to parse multiple let declarations"

-- ============================================================================
-- ASSERTION TESTS
-- ============================================================================

assertionBasicBool :: Test
assertionBasicBool = TestCase $ do
  let input = "assert true"
  case parse Parser.program "" input of
    Right (Program [Assert _]) -> return ()
    _ -> assertFailure "Failed to parse basic assertion"

assertionFalse :: Test
assertionFalse = TestCase $ do
  let input = "assert false"
  case parse Parser.program "" input of
    Right (Program [Assert _]) -> return ()
    _ -> assertFailure "Failed to parse assertion with false"

assertionIdentifier :: Test
assertionIdentifier = TestCase $ do
  let input = "assert x"
  case parse Parser.program "" input of
    Right (Program [Assert _]) -> return ()
    _ -> assertFailure "Failed to parse assertion with identifier"

assertionLogicalOp :: Test
assertionLogicalOp = TestCase $ do
  let input = "assert a /\\ b"
  case parse Parser.program "" input of
    Right (Program [Assert _]) -> return ()
    _ -> assertFailure "Failed to parse assertion with logical op"

assertionComparison :: Test
assertionComparison = TestCase $ do
  let input = "assert x > 5"
  case parse Parser.program "" input of
    Right (Program [Assert _]) -> return ()
    _ -> assertFailure "Failed to parse assertion with comparison"

assertionNegation :: Test
assertionNegation = TestCase $ do
  let input = "assert ~ x"
  case parse Parser.program "" input of
    Right (Program [Assert _]) -> return ()
    _ -> assertFailure "Failed to parse assertion with negation"

assertionComplex :: Test
assertionComplex = TestCase $ do
  let input = "assert (x > 0) /\\ (y < 10) => z = 5"
  case parse Parser.program "" input of
    Right (Program [Assert _]) -> return ()
    _ -> assertFailure "Failed to parse complex assertion"

assertionMultiple :: Test
assertionMultiple = TestCase $ do
  let input = "assert true\nassert false\nassert x"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 3 assertions" 3 (length stmts)
    _ -> assertFailure "Failed to parse multiple assertions"

-- ============================================================================
-- PROGRAM TESTS
-- ============================================================================

emptyProgram :: Test
emptyProgram = TestCase $ do
  let input = ""
  case parse Parser.program "" input of
    Right (Program []) -> return ()
    _ -> assertFailure "Failed to parse empty program"

programSingleVar :: Test
programSingleVar = TestCase $ do
  let input = "var x : int"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 1 statement" 1 (length stmts)
    _ -> assertFailure "Failed to parse program with single var"

programMixedStatements :: Test
programMixedStatements = TestCase $ do
  let input = "var x : int\nlet y = 5\nassert true"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 3 statements" 3 (length stmts)
    _ -> assertFailure "Failed to parse program with mixed statements"

programAllVars :: Test
programAllVars = TestCase $ do
  let input = "var a : bool\nvar b : int"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 2 var declarations" 2 (length stmts)
    _ -> assertFailure "Failed to parse program with all vars"

programAllLets :: Test
programAllLets = TestCase $ do
  let input = "let a = true\nlet b = 10\nlet c = 3.14"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 3 let declarations" 3 (length stmts)
    _ -> assertFailure "Failed to parse program with all lets"

programAllAsserts :: Test
programAllAsserts = TestCase $ do
  let input = "assert true\nassert false\nassert x = 5"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 3 assertions" 3 (length stmts)
    _ -> assertFailure "Failed to parse program with all asserts"

programLargeComplex :: Test
programLargeComplex = TestCase $ do
  let input = "var x : int\nvar y : bool\nlet z = x + 5\nassert z > 0\nlet w = true /\\ y\nassert w"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 6 statements" 6 (length stmts)
    _ -> assertFailure "Failed to parse large complex program"

-- ============================================================================
-- WHITESPACE TESTS
-- ============================================================================

whitespaceLeading :: Test
whitespaceLeading = TestCase $ do
  let input = "   var x : int"
  case parse Parser.program "" input of
    Right (Program [Var "x" Int]) -> return ()
    _ -> assertFailure "Failed to parse with leading whitespace"

whitespaceTrailing :: Test
whitespaceTrailing = TestCase $ do
  let input = "var x : int   "
  case parse Parser.program "" input of
    Right (Program [Var "x" Int]) -> return ()
    _ -> assertFailure "Failed to parse with trailing whitespace"

whitespaceExcessive :: Test
whitespaceExcessive = TestCase $ do
  let input = "var   x   :   int"
  case parse Parser.program "" input of
    Right (Program [Var "x" Int]) -> return ()
    _ -> assertFailure "Failed to parse with excessive whitespace"

whitespaceTabs :: Test
whitespaceTabs = TestCase $ do
  let input = "var\tx\t:\tint"
  case parse Parser.program "" input of
    Right (Program [Var "x" Int]) -> return ()
    _ -> assertFailure "Failed to parse with tabs"

whitespaceNewlines :: Test
whitespaceNewlines = TestCase $ do
  let input = "var x : int\n\n\nlet y = 5"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 2 statements" 2 (length stmts)
    _ -> assertFailure "Failed to parse with multiple newlines"

-- ============================================================================
-- COMMENT TESTS
-- ============================================================================

commentLineSimple :: Test
commentLineSimple = TestCase $ do
  let input = "var x : int -- this is a comment"
  case parse Parser.program "" input of
    Right (Program [Var "x" Int]) -> return ()
    _ -> assertFailure "Failed to parse with line comment"

commentLineMultiple :: Test
commentLineMultiple = TestCase $ do
  let input = "var x : int -- comment 1\nlet y = 5 -- comment 2"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 2 statements" 2 (length stmts)
    _ -> assertFailure "Failed to parse with multiple line comments"

commentBlockSimple :: Test
commentBlockSimple = TestCase $ do
  let input = "var x : int {- block comment -}"
  case parse Parser.program "" input of
    Right (Program [Var "x" Int]) -> return ()
    _ -> assertFailure "Failed to parse with block comment"

commentBlockNested :: Test
commentBlockNested = TestCase $ do
  let input = "var x : int {- outer {- inner -} outer -}"
  case parse Parser.program "" input of
    Right (Program [Var "x" Int]) -> return ()
    _ -> assertFailure "Failed to parse with nested block comments"

commentBlockMultiline :: Test
commentBlockMultiline = TestCase $ do
  let input = "var x : int\n{- multi\n   line\n   comment -}\nlet y = 5"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 2 statements" 2 (length stmts)
    _ -> assertFailure "Failed to parse with multiline block comment"

-- ============================================================================
-- COMPLEX EXPRESSION TESTS
-- ============================================================================

complexExprMultiLevel :: Test
complexExprMultiLevel = TestCase $ do
  let input = "let x = ((5 + 3) * 2 - 1) / 4"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse multi-level complex expression"

complexExprMixedTypes :: Test
complexExprMixedTypes = TestCase $ do
  let input = "let x = (5 < 10) /\\ (x = y)"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse mixed type expression"

complexExprAllOps :: Test
complexExprAllOps = TestCase $ do
  let input = "let x = a + b - c * d / e /\\ f \\/ g => h"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse expression with all operators"

complexExprDeepNesting :: Test
complexExprDeepNesting = TestCase $ do
  let input = "let x = (((a + b) * (c - d)) / (e + (f * g)))"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse deeply nested expression"

complexExprWithUnary :: Test
complexExprWithUnary = TestCase $ do
  let input = "let x = - (5 + 3) * ~ true"
  case parse Parser.program "" input of
    Right (Program [Let "x" _]) -> return ()
    _ -> assertFailure "Failed to parse expression with multiple unary ops"

-- ============================================================================
-- COMBINED TESTS - Multiple features
-- ============================================================================

combinedVarAndLet :: Test
combinedVarAndLet = TestCase $ do
  let input = "var x : int\nlet y = x + 5"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 2 statements" 2 (length stmts)
    _ -> assertFailure "Failed to parse var and let together"

combinedLetAndAssert :: Test
combinedLetAndAssert = TestCase $ do
  let input = "let x = 5\nassert x > 0"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 2 statements" 2 (length stmts)
    _ -> assertFailure "Failed to parse let and assert together"

combinedAllStatements :: Test
combinedAllStatements = TestCase $ do
  let input = "var x : int\nvar y : bool\nlet z = x + 5\nassert y"
  case parse Parser.program "" input of
    Right (Program stmts) ->
      assertEqual "Should have 4 statements" 4 (length stmts)
    _ -> assertFailure "Failed to parse all statement types"

combinedExprWithAllOps :: Test
combinedExprWithAllOps = TestCase $ do
  let input = "let result = (5 + 3 * 2) / (10 - 6) = 2"
  case parse Parser.program "" input of
    Right (Program [Let "result" _]) -> return ()
    _ -> assertFailure "Failed to parse expression combining all operations"

-- ============================================================================
-- TEST SUITE
-- ============================================================================

main :: IO ()
main = do
  let tests =
        [ -- Type tests
          TestLabel "typeTestsBool" typeTestsBool
        , TestLabel "typesTestsInt" typesTestsInt
        , -- Variable declaration tests
          TestLabel "varDeclSimple" varDeclSimple
        , TestLabel "varDeclMultiChar" varDeclMultiChar
        , TestLabel "varDeclMultiple" varDeclMultiple
        , TestLabel "varDeclWithWhitespace" varDeclWithWhitespace
        , -- Boolean literal tests
          TestLabel "boolLitTrue" boolLitTrue
        , TestLabel "boolLitFalse" boolLitFalse
        , -- Integer literal tests
          TestLabel "intLitZero" intLitZero
        , TestLabel "intLitPositive" intLitPositive
        , TestLabel "intLitLarge" intLitLarge
        , TestLabel "intLitNegative" intLitNegative
        , TestLabel "intLitNegativeZero" intLitNegativeZero
        , -- Unary operator tests
          TestLabel "unaryNotBool" unaryNotBool
        , TestLabel "unaryNotIdentifier" unaryNotIdentifier
        , TestLabel "unaryNotNested" unaryNotNested
        , TestLabel "unaryNegateInt" unaryNegateInt
        , TestLabel "unaryNegateIdentifier" unaryNegateIdentifier
        , TestLabel "unaryNegateNested" unaryNegateNested
        , -- Binary and operator tests
          TestLabel "binaryAndBasic" binaryAndBasic
        , TestLabel "binaryAndChained" binaryAndChained
        , TestLabel "binaryAndWithIdentifiers" binaryAndWithIdentifiers
        , -- Binary or operator tests
          TestLabel "binaryOrBasic" binaryOrBasic
        , TestLabel "binaryOrChained" binaryOrChained
        , TestLabel "binaryOrWithIdentifiers" binaryOrWithIdentifiers
        , -- Binary implies operator tests
          TestLabel "binaryImpliesBasic" binaryImpliesBasic
        , TestLabel "binaryImpliesChained" binaryImpliesChained
        , -- Binary iff operator tests
          TestLabel "binaryIffBasic" binaryIffBasic
        , TestLabel "binaryIffChained" binaryIffChained
        , -- Equality operator tests
          TestLabel "equalityBool" equalityBool
        , TestLabel "equalityInt" equalityInt
        , TestLabel "equalityChained" equalityChained
        , -- Inequality operator tests
          TestLabel "inequalityBool" inequalityBool
        , TestLabel "inequalityInt" inequalityInt
        , -- Relational operator tests
          TestLabel "lessThanInt" lessThanInt
        , TestLabel "greaterThanInt" greaterThanInt
        , TestLabel "lessThanOrEqualInt" lessThanOrEqualInt
        , TestLabel "greaterThanOrEqualInt" greaterThanOrEqualInt
        , -- Arithmetic addition tests
          TestLabel "addIntBasic" addIntBasic
        , TestLabel "addIntChained" addIntChained
        , TestLabel "addIntNegative" addIntNegative
        , -- Arithmetic subtraction tests
          TestLabel "subIntBasic" subIntBasic
        , TestLabel "subIntChained" subIntChained
        , -- Arithmetic multiplication tests
          TestLabel "mulIntBasic" mulIntBasic
        , TestLabel "mulIntChained" mulIntChained
        , -- Arithmetic division tests
          TestLabel "divIntBasic" divIntBasic
        , -- Operator precedence tests
          TestLabel "precedenceAddVsMul" precedenceAddVsMul
        , TestLabel "precedenceMulVsAdd" precedenceMulVsAdd
        , TestLabel "precedenceAddVsSub" precedenceAddVsSub
        , TestLabel "precedenceComparisonVsArithmetic" precedenceComparisonVsArithmetic
        , TestLabel "precedenceAndVsOr" precedenceAndVsOr
        , TestLabel "precedenceOrVsImplies" precedenceOrVsImplies
        , TestLabel "precedenceComparisonVsLogical" precedenceComparisonVsLogical
        , TestLabel "precedenceUnaryVsBinary" precedenceUnaryVsBinary
        , -- Parentheses tests
          TestLabel "parenthesesSimple" parenthesesSimple
        , TestLabel "parenthesesBinaryOp" parenthesesBinaryOp
        , TestLabel "parenthesesOverridePrecedence" parenthesesOverridePrecedence
        , TestLabel "parenthesesNested" parenthesesNested
        , TestLabel "parenthesesDeeplyNested" parenthesesDeeplyNested
        , TestLabel "parenthesesUnary" parenthesesUnary
        , TestLabel "parenthesesLogical" parenthesesLogical
        , -- Identifier tests
          TestLabel "identifierSingleChar" identifierSingleChar
        , TestLabel "identifierMultiChar" identifierMultiChar
        , TestLabel "identifierWithUnderscore" identifierWithUnderscore
        , TestLabel "identifierWithNumbers" identifierWithNumbers
        , TestLabel "identifierStartsWithLetter" identifierStartsWithLetter
        , TestLabel "identifierInExpression" identifierInExpression
        , TestLabel "identifierInComparison" identifierInComparison
        , -- Let declaration tests
          TestLabel "letDeclBasic" letDeclBasic
        , TestLabel "letDeclBool" letDeclBool
        , TestLabel "letDeclComplex" letDeclComplex
        , TestLabel "letDeclLogical" letDeclLogical
        , TestLabel "letDeclMultiple" letDeclMultiple
        , -- Assertion tests
          TestLabel "assertionBasicBool" assertionBasicBool
        , TestLabel "assertionFalse" assertionFalse
        , TestLabel "assertionIdentifier" assertionIdentifier
        , TestLabel "assertionLogicalOp" assertionLogicalOp
        , TestLabel "assertionComparison" assertionComparison
        , TestLabel "assertionNegation" assertionNegation
        , TestLabel "assertionComplex" assertionComplex
        , TestLabel "assertionMultiple" assertionMultiple
        , -- Program tests
          TestLabel "emptyProgram" emptyProgram
        , TestLabel "programSingleVar" programSingleVar
        , TestLabel "programMixedStatements" programMixedStatements
        , TestLabel "programAllVars" programAllVars
        , TestLabel "programAllLets" programAllLets
        , TestLabel "programAllAsserts" programAllAsserts
        , TestLabel "programLargeComplex" programLargeComplex
        , -- Whitespace tests
          TestLabel "whitespaceLeading" whitespaceLeading
        , TestLabel "whitespaceTrailing" whitespaceTrailing
        , TestLabel "whitespaceExcessive" whitespaceExcessive
        , TestLabel "whitespaceTabs" whitespaceTabs
        , TestLabel "whitespaceNewlines" whitespaceNewlines
        , -- Comment tests
          TestLabel "commentLineSimple" commentLineSimple
        , TestLabel "commentLineMultiple" commentLineMultiple
        , TestLabel "commentBlockSimple" commentBlockSimple
        , TestLabel "commentBlockNested" commentBlockNested
        , TestLabel "commentBlockMultiline" commentBlockMultiline
        , -- Complex expression tests
          TestLabel "complexExprMultiLevel" complexExprMultiLevel
        , TestLabel "complexExprMixedTypes" complexExprMixedTypes
        , TestLabel "complexExprAllOps" complexExprAllOps
        , TestLabel "complexExprDeepNesting" complexExprDeepNesting
        , TestLabel "complexExprWithUnary" complexExprWithUnary
        , -- Combined tests
          TestLabel "combinedVarAndLet" combinedVarAndLet
        , TestLabel "combinedLetAndAssert" combinedLetAndAssert
        , TestLabel "combinedAllStatements" combinedAllStatements
        , TestLabel "combinedExprWithAllOps" combinedExprWithAllOps
        ]

  putStrLn "Running SMTL Parser Test Suite..."
  putStrLn $ "Total tests: " ++ show (length tests)
  putStrLn ""

  result <- runTestTT (TestList tests)

  if failures result == 0 && errors result == 0
    then putStrLn "\nAll tests passed!"
    else putStrLn $ "\nTests failed: " ++ show (failures result) ++ " failures, " ++ show (errors result) ++ " errors"
