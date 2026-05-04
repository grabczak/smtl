{-# LANGUAGE GADTs #-}

module Parser (parseProgram) where

import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST

type Error = Void
type Input = String
type Parser = Parsec Error Input

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reservedWords :: [String]
reservedWords =
  [ "var"
  , "let"
  , "assert"
  , "bool"
  , "int"
  , "T"
  , "F"
  ]

keyword :: String -> Parser ()
keyword kw = lexeme $ do
  _ <- string kw
  notFollowedBy alphaNumChar

identifier :: Parser Identifier
identifier = lexeme $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_')
  let name = first : rest
  if name `elem` reservedWords
    then fail $ "Keyword '" ++ name ++ "' cannot be used as an identifier"
    else return name

bool, int :: Parser UType
bool = lexeme $ keyword "bool" >> return UBool
int = lexeme $ keyword "int" >> return UInt

var :: Parser UExpr
var = lexeme $ do
  v <- identifier
  return $ UVar v

boolLit :: Parser UExpr
boolLit = lexeme $ do
  b <- (keyword "T" >> return True) <|> (keyword "F" >> return False)
  return $ UBoolLit b

intLit :: Parser UExpr
intLit = lexeme $ do
  n <- L.decimal
  return $ UIntLit n

operatorTable :: [[Operator Parser UExpr]]
operatorTable =
  [
    [ Prefix (symbol "~" >> return UNot)
    , Prefix (symbol "-" >> return UNeg)
    ]
  , [InfixL (symbol "*" >> return UMul)]
  ,
    [ InfixL (symbol "+" >> return UAdd)
    , InfixL (symbol "-" >> return USub)
    ]
  ,
    [ InfixN (try (symbol "==") >> return UEq)
    , InfixN (try (symbol "!=") >> return UNeq)
    , InfixN (try (symbol "<=>") >> return UIff)
    , InfixN (try (symbol "<=") >> return ULeq)
    , InfixN (try (symbol ">=") >> return UGeq)
    , InfixN (symbol "<" >> return ULt)
    , InfixN (symbol ">" >> return UGt)
    ]
  , [InfixL (symbol "/\\" >> return UAnd)]
  , [InfixL (symbol "\\/" >> return UOr)]
  , [InfixR (try (symbol "=>") >> return UImplies)]
  ]

term :: Parser UExpr
term =
  choice
    [ parens expr
    , boolLit
    , intLit
    , var
    ]

expr :: Parser UExpr
expr = makeExprParser term operatorTable

declareVar :: Parser UStatement
declareVar = lexeme $ do
  keyword "var"
  v <- identifier
  _ <- symbol ":"
  t <- bool <|> int
  return $ UDeclareVar v t

letBinding :: Parser UStatement
letBinding = lexeme $ do
  keyword "let"
  v <- identifier
  _ <- symbol "="
  e <- expr
  return $ ULetBinding v e

assertion :: Parser UStatement
assertion = lexeme $ do
  keyword "assert"
  e <- expr
  return $ UAssertion e

statement :: Parser UStatement
statement = do
  st <- declareVar <|> letBinding <|> assertion
  return st

program :: Parser UProgram
program = do
  sc
  stmts <- many statement
  eof
  return $ UProgram stmts

parseProgram :: String -> String -> Either (ParseErrorBundle Input Error) UProgram
parseProgram path content = parse program path content
