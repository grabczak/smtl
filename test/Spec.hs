import Test.HUnit
import qualified ParserSpec
import qualified TypeCheckerSpec
import qualified SolverSpec

main :: IO ()
main = do
  result <- runTestTT allTests
  if failures result > 0 || errors result > 0
    then fail "Tests failed"
    else return ()

allTests :: Test
allTests = TestList
  [ TestLabel "Parser Tests" ParserSpec.tests
  , TestLabel "TypeChecker Tests" TypeCheckerSpec.tests
  , TestLabel "Solver Tests" SolverSpec.tests
  ]
