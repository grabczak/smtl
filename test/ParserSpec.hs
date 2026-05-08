module ParserSpec (tests) where

import Test.HUnit
import Text.Megaparsec (errorBundlePretty)

import AST (Program (..))
import Parser (parseProgram)

-- ============================================================================
-- Parser Tests
-- ============================================================================

-- Basic declarations
testParseSimpleDeclaration :: Test
testParseSimpleDeclaration = TestCase $ do
  let input = "var x : int"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse simple declaration"
    Right _ -> assertBool "Parsed simple declaration" True

testParseMultipleDeclarations :: Test
testParseMultipleDeclarations = TestCase $ do
  let input = "var x, y, z : int"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse multiple declarations"
    Right _ -> assertBool "Parsed multiple declarations" True

testParseBoolDeclaration :: Test
testParseBoolDeclaration = TestCase $ do
  let input = "var flag : bool"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse bool declaration"
    Right _ -> assertBool "Parsed bool declaration" True

testParseMixedTypes :: Test
testParseMixedTypes = TestCase $ do
  let input = "var x : int\nvar flag : bool"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse mixed types"
    Right _ -> assertBool "Parsed mixed types" True

-- Simple assignments
testParseAssignLiteral :: Test
testParseAssignLiteral = TestCase $ do
  let input = "let x = 42"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse literal assignment"
    Right _ -> assertBool "Parsed literal assignment" True

testParseAssignBoolLiteral :: Test
testParseAssignBoolLiteral = TestCase $ do
  let input = "let flag = T"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse bool assignment"
    Right _ -> assertBool "Parsed bool assignment" True

testParseAssignVariable :: Test
testParseAssignVariable = TestCase $ do
  let input = "let y = x"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse variable assignment"
    Right _ -> assertBool "Parsed variable assignment" True

-- Arithmetic expressions
testParseAddition :: Test
testParseAddition = TestCase $ do
  let input = "let z = x + y"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse addition"
    Right _ -> assertBool "Parsed addition" True

testParseSubtraction :: Test
testParseSubtraction = TestCase $ do
  let input = "let z = x - y"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse subtraction"
    Right _ -> assertBool "Parsed subtraction" True

testParseMultiplication :: Test
testParseMultiplication = TestCase $ do
  let input = "let z = x * y"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse multiplication"
    Right _ -> assertBool "Parsed multiplication" True

testParseNegation :: Test
testParseNegation = TestCase $ do
  let input = "let z = -x"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse negation"
    Right _ -> assertBool "Parsed negation" True

-- Operator precedence
testParsePrecedenceMultBeforeAdd :: Test
testParsePrecedenceMultBeforeAdd = TestCase $ do
  let input = "let z = 2 + 3 * 4"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse precedence test"
    Right _ -> assertBool "Parsed multiplication before addition" True

testParsePrecedenceWithParens :: Test
testParsePrecedenceWithParens = TestCase $ do
  let input = "let z = (2 + 3) * 4"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse parenthesized expression"
    Right _ -> assertBool "Parsed parenthesized expression" True

-- Comparison operators
testParseEquality :: Test
testParseEquality = TestCase $ do
  let input = "let c = x == y"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse equality"
    Right _ -> assertBool "Parsed equality" True

testParseInequality :: Test
testParseInequality = TestCase $ do
  let input = "let c = x != y"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse inequality"
    Right _ -> assertBool "Parsed inequality" True

testParseLessThan :: Test
testParseLessThan = TestCase $ do
  let input = "let c = x < y"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse less than"
    Right _ -> assertBool "Parsed less than" True

testParseGreaterThan :: Test
testParseGreaterThan = TestCase $ do
  let input = "let c = x > y"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse greater than"
    Right _ -> assertBool "Parsed greater than" True

testParseLessEqual :: Test
testParseLessEqual = TestCase $ do
  let input = "let c = x <= y"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse less or equal"
    Right _ -> assertBool "Parsed less or equal" True

testParseGreaterEqual :: Test
testParseGreaterEqual = TestCase $ do
  let input = "let c = x >= y"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse greater or equal"
    Right _ -> assertBool "Parsed greater or equal" True

-- Logical operators
testParseAnd :: Test
testParseAnd = TestCase $ do
  let input = "let c = a /\\ b"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse AND"
    Right _ -> assertBool "Parsed AND" True

testParseOr :: Test
testParseOr = TestCase $ do
  let input = "let c = a \\/ b"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse OR"
    Right _ -> assertBool "Parsed OR" True

testParseNot :: Test
testParseNot = TestCase $ do
  let input = "let c = ~a"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse NOT"
    Right _ -> assertBool "Parsed NOT" True

testParseImplies :: Test
testParseImplies = TestCase $ do
  let input = "let c = a => b"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse implies"
    Right _ -> assertBool "Parsed implies" True

testParseIff :: Test
testParseIff = TestCase $ do
  let input = "let c = a <=> b"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse iff"
    Right _ -> assertBool "Parsed iff" True

-- Assertions
testParseSimpleAssert :: Test
testParseSimpleAssert = TestCase $ do
  let input = "assert T"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse simple assertion"
    Right _ -> assertBool "Parsed simple assertion" True

testParseAssertComparison :: Test
testParseAssertComparison = TestCase $ do
  let input = "assert x >= 0"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse comparison assertion"
    Right _ -> assertBool "Parsed comparison assertion" True

testParseAssertLogical :: Test
testParseAssertLogical = TestCase $ do
  let input = "assert x >= 0 /\\ x <= 100"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse logical assertion"
    Right _ -> assertBool "Parsed logical assertion" True

-- Comments
testParseWithLineComment :: Test
testParseWithLineComment = TestCase $ do
  let input = "# This is a comment\nvar x : int"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse with line comment"
    Right _ -> assertBool "Parsed with line comment" True

testParseWithCommentAfter :: Test
testParseWithCommentAfter = TestCase $ do
  let input = "var x : int # inline comment"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse with inline comment"
    Right _ -> assertBool "Parsed with inline comment" True

-- Complex programs
testParseCompleteProgram :: Test
testParseCompleteProgram = TestCase $ do
  let input = "var x, y : int\nlet z = x + y\nassert z >= 0"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse complete program"
    Right _ -> assertBool "Parsed complete program" True

testParseProductionExample :: Test
testParseProductionExample = TestCase $ do
  let input = "var product_a, product_b, product_c : int\nvar total_profit : int\nlet labor_constraint = 2 * product_a + 3 * product_b + 1 * product_c <= 40\nassert labor_constraint"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse production example"
    Right _ -> assertBool "Parsed production example" True

testParseEmpty :: Test
testParseEmpty = TestCase $ do
  let input = ""
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse empty program"
    Right _ -> assertBool "Parsed empty program" True

testParseOnlyComments :: Test
testParseOnlyComments = TestCase $ do
  let input = "# just comments\n# more comments"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse comment-only program"
    Right _ -> assertBool "Parsed comment-only program" True

-- Variable names
testParseVarWithUnderscore :: Test
testParseVarWithUnderscore = TestCase $ do
  let input = "var labor_hours_used : int"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse variable with underscore"
    Right _ -> assertBool "Parsed variable with underscore" True

testParseVarWithNumbers :: Test
testParseVarWithNumbers = TestCase $ do
  let input = "var var123 : int"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse variable with numbers"
    Right _ -> assertBool "Parsed variable with numbers" True

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
    Left _ -> assertFailure "Failed to parse large number"
    Right _ -> assertBool "Parsed large number" True

-- Negative numbers
testParseNegativeNumber :: Test
testParseNegativeNumber = TestCase $ do
  let input = "let x = -42"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse negative number"
    Right _ -> assertBool "Parsed negative number" True

testParseNestedNegation :: Test
testParseNestedNegation = TestCase $ do
  let input = "let x = -(-(5))"
  case parseProgram "test" input of
    Left _ -> assertFailure "Failed to parse nested negation"
    Right _ -> assertBool "Parsed nested negation" True

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
