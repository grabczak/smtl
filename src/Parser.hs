{-# LANGUAGE GADTs #-}

module Parser (
  Identifier,
  Type (..),
  Expr (..),
  Statement (..),
  Program (..),
  program,
) where

import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Primitives

type Parser = Parsec Void String

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

-- AST

type Identifier = String

data Type = Bool | Int
  deriving (Show)

-- Untyped expression, used during parsing before type checking
data Expr
  = Var Identifier
  | BoolLit Bool
  | IntLit Int
  | Not Expr
  | Neg Expr
  | And Expr Expr
  | Or Expr Expr
  | Implies Expr Expr
  | Iff Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Leq Expr Expr
  | Geq Expr Expr
  deriving (Show)

data Statement
  = VarDeclaration Identifier Type
  | LetBinding Identifier Expr
  | Assertion Expr
  deriving (Show)

data Program = Program [Statement]

instance Show Program where
  show (Program stmts) = unlines $ map show stmts

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

bool, int :: Parser Type
bool = lexeme $ keyword "bool" >> return Bool
int = lexeme $ keyword "int" >> return Int

var :: Parser Expr
var = lexeme $ do
  v <- identifier
  return $ Var v

boolLit :: Parser Expr
boolLit = lexeme $ do
  b <- (keyword "T" >> return True) <|> (keyword "F" >> return False)
  return $ BoolLit b

intLit :: Parser Expr
intLit = lexeme $ do
  n <- L.decimal
  return $ IntLit n

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ Prefix (symbol "~" >> return Not)
    , Prefix (symbol "-" >> return Neg)
    ]
  , [InfixL (symbol "*" >> return Mul)]
  ,
    [ InfixL (symbol "+" >> return Add)
    , InfixL (symbol "-" >> return Sub)
    ]
  ,
    [ InfixN (try (symbol "==") >> return Eq)
    , InfixN (try (symbol "!=") >> return Neq)
    , InfixN (try (symbol "<=>") >> return Iff)
    , InfixN (try (symbol "<=") >> return Leq)
    , InfixN (try (symbol ">=") >> return Geq)
    , InfixN (symbol "<" >> return Lt)
    , InfixN (symbol ">" >> return Gt)
    ]
  , [InfixL (symbol "/\\" >> return And)]
  , [InfixL (symbol "\\/" >> return Or)]
  , [InfixR (try (symbol "=>") >> return Implies)]
  ]

term :: Parser Expr
term =
  choice
    [ parens expr
    , boolLit
    , intLit
    , var
    ]

expr :: Parser Expr
expr = makeExprParser term operatorTable

varDeclaration :: Parser Statement
varDeclaration = lexeme $ do
  keyword "var"
  v <- identifier
  _ <- symbol ":"
  t <- bool <|> int
  return $ VarDeclaration v t

letBinding :: Parser Statement
letBinding = lexeme $ do
  keyword "let"
  v <- identifier
  _ <- symbol "="
  e <- expr
  return $ LetBinding v e

assertion :: Parser Statement
assertion = lexeme $ do
  keyword "assert"
  e <- expr
  return $ Assertion e

-- Entry point

statement :: Parser Statement
statement = do
  st <- varDeclaration <|> letBinding <|> assertion
  return st

program :: Parser Program
program = do
  sc
  stmts <- many statement
  eof
  return $ Program stmts
