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

data Type = Bool | Int deriving (Show)

data Expr a where
  -- Booleans
  BoolLit :: Bool -> Expr Bool
  BoolVar :: Identifier -> Expr Bool
  BoolNot :: Expr Bool -> Expr Bool
  BoolAnd :: Expr Bool -> Expr Bool -> Expr Bool
  BoolOr :: Expr Bool -> Expr Bool -> Expr Bool
  BoolImplies :: Expr Bool -> Expr Bool -> Expr Bool
  BoolIff :: Expr Bool -> Expr Bool -> Expr Bool
  -- Integers
  IntLit :: Int -> Expr Int
  IntVar :: Identifier -> Expr Int
  IntAdd :: Expr Int -> Expr Int -> Expr Int
  IntSub :: Expr Int -> Expr Int -> Expr Int
  IntMul :: Expr Int -> Expr Int -> Expr Int
  -- Comparisons
  Eq :: Expr a -> Expr a -> Expr Bool
  Neq :: Expr a -> Expr a -> Expr Bool
  Lt :: Expr a -> Expr a -> Expr Bool
  Gt :: Expr a -> Expr a -> Expr Bool
  Leq :: Expr a -> Expr a -> Expr Bool
  Geq :: Expr a -> Expr a -> Expr Bool

instance Show (Expr a) where
  show (BoolLit b) = "BoolLit " ++ show b
  show (BoolVar v) = "BoolVar " ++ v
  show (BoolNot e) = "BoolNot (" ++ show e ++ ")"
  show (BoolOr a b) = "BoolOr (" ++ show a ++ ") (" ++ show b ++ ")"
  show (BoolAnd a b) = "BoolAnd (" ++ show a ++ ") (" ++ show b ++ ")"
  show (BoolImplies a b) = "BoolImpl (" ++ show a ++ ") (" ++ show b ++ ")"
  show (BoolIff a b) = "BoolIff (" ++ show a ++ ") (" ++ show b ++ ")"
  show (IntLit n) = "IntLit " ++ show n
  show (IntVar v) = "IntVar " ++ v
  show (IntAdd a b) = "IntAdd (" ++ show a ++ ") (" ++ show b ++ ")"
  show (IntSub a b) = "IntSub (" ++ show a ++ ") (" ++ show b ++ ")"
  show (IntMul a b) = "IntMul (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Eq a b) = "Eq (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Neq a b) = "Neq (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Lt a b) = "Lt (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Gt a b) = "Gt (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Leq a b) = "Leq (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Geq a b) = "Geq (" ++ show a ++ ") (" ++ show b ++ ")"

data Statement
  = Var Identifier Type
  | Let Identifier (Expr Bool)
  | Assert (Expr Bool)
  deriving (Show)

data Program = Program [Statement]

instance Show Program where
  show (Program stmts) = "Program [\n" ++ concatMap (\s -> "  " ++ show s ++ "\n") stmts ++ "]"

reservedWords :: [String]
reservedWords =
  [ "var"
  , "let"
  , "assert"
  , "bool"
  , "T"
  , "F"
  , "int"
  , "real"
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

varStmt :: Parser Statement
varStmt = lexeme $ do
  keyword "var"
  v <- identifier
  _ <- symbol ":"
  t <- bool <|> int
  return $ Var v t

boolLit :: Parser (Expr Bool)
boolLit = lexeme $ do
  b <- (keyword "T" >> return True) <|> (keyword "F" >> return False)
  return $ BoolLit b

boolVar :: Parser (Expr Bool)
boolVar = lexeme $ do
  v <- identifier
  return $ BoolVar v

boolNot :: Parser (Expr Bool)
boolNot = lexeme $ do
  _ <- symbol "~"
  p <- boolAtom
  return $ BoolNot p

boolAtom :: Parser (Expr Bool)
boolAtom =
  choice
    [ boolLit
    , boolNot
    , boolVar
    , parens boolExpr
    ]

boolOperatorTable :: [[Operator Parser (Expr Bool)]]
boolOperatorTable =
  [ [InfixL (BoolAnd <$ symbol "/\\")]
  , [InfixL (BoolOr <$ symbol "\\/")]
  , [InfixR (BoolImplies <$ symbol "=>")]
  , [InfixN (BoolIff <$ symbol "<=>")]
  ]

boolExpr :: Parser (Expr Bool)
boolExpr = makeExprParser boolAtom boolOperatorTable

letStmt :: Parser Statement
letStmt = lexeme $ do
  keyword "let"
  v <- identifier
  _ <- symbol "="
  e <- boolExpr
  return $ Let v e

-- Entry point

statement :: Parser Statement
statement = do
  st <- varStmt <|> letStmt
  return st

program :: Parser Program
program = do
  sc
  stmts <- many statement
  eof
  return $ Program stmts
