{-# LANGUAGE GADTs #-}

module Parser (Program, program) where

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

-- AST

type Identifier = String

data Type = Bool | Int | Real deriving (Show)

data Expr a where
  -- Booleans
  BoolLit :: Bool -> Expr Bool
  BoolNot :: Expr Bool -> Expr Bool
  BoolOr :: Expr Bool -> Expr Bool -> Expr Bool
  BoolAnd :: Expr Bool -> Expr Bool -> Expr Bool
  BoolImplies :: Expr Bool -> Expr Bool -> Expr Bool
  BoolIff :: Expr Bool -> Expr Bool -> Expr Bool -> Expr Bool
  -- Integers
  IntLit :: Int -> Expr Int
  IntAdd :: Expr Int -> Expr Int -> Expr Int
  IntSub :: Expr Int -> Expr Int -> Expr Int
  IntMul :: Expr Int -> Expr Int -> Expr Int
  -- Reals
  RealLit :: Double -> Expr Double
  RealAdd :: Expr Double -> Expr Double -> Expr Double
  RealSub :: Expr Double -> Expr Double -> Expr Double
  RealMul :: Expr Double -> Expr Double -> Expr Double
  RealDiv :: Expr Double -> Expr Double -> Expr Double
  -- Comparisons
  Eq :: Expr a -> Expr a -> Expr Bool
  Neq :: Expr a -> Expr a -> Expr Bool
  Lt :: Expr a -> Expr a -> Expr Bool
  Gt :: Expr a -> Expr a -> Expr Bool
  Leq :: Expr a -> Expr a -> Expr Bool
  Geq :: Expr a -> Expr a -> Expr Bool

instance Show (Expr a) where
  show (BoolLit b) = "BoolLit " ++ show b
  show (BoolNot e) = "BoolNot (" ++ show e ++ ")"
  show (BoolAnd a b) = "BoolAnd (" ++ show a ++ ") (" ++ show b ++ ")"
  show (BoolOr a b) = "BoolOr (" ++ show a ++ ") (" ++ show b ++ ")"
  show (BoolImplies a b) = "BoolImpl (" ++ show a ++ ") (" ++ show b ++ ")"
  show (BoolIff a b c) = "BoolIff (" ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"
  show (IntLit n) = "IntLit " ++ show n
  show (IntAdd a b) = "IntAdd (" ++ show a ++ ") (" ++ show b ++ ")"
  show (IntSub a b) = "IntSub (" ++ show a ++ ") (" ++ show b ++ ")"
  show (IntMul a b) = "IntMul (" ++ show a ++ ") (" ++ show b ++ ")"
  show (RealLit r) = "RealLit " ++ show r
  show (RealAdd a b) = "RealAdd (" ++ show a ++ ") (" ++ show b ++ ")"
  show (RealSub a b) = "RealSub (" ++ show a ++ ") (" ++ show b ++ ")"
  show (RealMul a b) = "RealMul (" ++ show a ++ ") (" ++ show b ++ ")"
  show (RealDiv a b) = "RealDiv (" ++ show a ++ ") (" ++ show b ++ ")"
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

data Program = Program [Statement] deriving (Show)

reservedWords :: [String]
reservedWords = ["var", "let", "assert", "bool", "int", "real"]

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

bool, int, real :: Parser Type
bool = lexeme $ keyword "bool" >> return Bool
int = lexeme $ keyword "int" >> return Int
real = lexeme $ keyword "real" >> return Real

var :: Parser Statement
var = lexeme $ do
  keyword "var"
  v <- identifier
  _ <- symbol ":"
  t <- bool <|> int <|> real
  return $ Var v t

-- Entry point

statement :: Parser Statement
statement = do
  st <- var
  return st

program :: Parser Program
program = do
  sc
  stmts <- many statement
  eof
  return $ Program stmts
