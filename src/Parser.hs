module Parser (parseProgram) where

import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST

-- Parser

type Error = Void
type Input = String
type Parser = Parsec Error Input

-- Primitives

-- Skips whitespace and comments (space consumer)
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "#")
    empty

-- Consumes trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parses a fixed string and consumes trailing whitespace
symbol :: String -> Parser String
symbol = L.symbol sc

-- Parentheses helper
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Keywords and identifiers

-- Reserved keywords
reserved :: [String]
reserved =
  [ "var"
  , "let"
  , "assert"
  , "bool"
  , "int"
  , "T"
  , "F"
  ]

-- Parses a keyword and ensures it's not followed by an alphanumeric character (to prevent partial matches)
keyword :: String -> Parser ()
keyword kw = lexeme $ do
  _ <- string kw
  notFollowedBy alphaNumChar

-- Parses an identifier, ensuring it doesn't match any reserved keyword
identifier :: Parser (Identifier)
identifier = lexeme $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_')
  let name = first : rest
  if name `elem` reserved
    then fail $ "Keyword '" ++ name ++ "' cannot be used as an identifier"
    else return name

-- Locators

-- Wraps a parser to include source position
locate :: Parser a -> Parser (Loc a)
locate par = do
  pos <- getSourcePos
  res <- par
  return $ Loc pos res

-- Puts a unary operator in a location wrapper
locateOp1 :: String -> (Loc Expr -> Expr) -> Parser (Loc Expr -> Loc Expr)
locateOp1 op constr = do
  pos <- getSourcePos
  _ <- symbol op
  return $ \e -> Loc pos (constr e)

-- Puts a binary operator in a location wrapper
locateOp2 :: String -> (Loc Expr -> Loc Expr -> Expr) -> Parser (Loc Expr -> Loc Expr -> Loc Expr)
locateOp2 op constr = do
  pos <- getSourcePos
  _ <- symbol op
  return $ \e1 e2 -> Loc pos (constr e1 e2)

-- Expressions and statements

-- Parses type keywords
bool, int :: Parser Type
bool = lexeme $ keyword "bool" >> return Bool
int = lexeme $ keyword "int" >> return Int

-- Parses variable identifiers
var :: Parser (Loc Expr)
var = locate $ lexeme $ do
  v <- identifier
  return $ Var v

-- Parses boolean literals
boolLit :: Parser (Loc Expr)
boolLit = locate $ lexeme $ do
  b <- (keyword "T" >> return True) <|> (keyword "F" >> return False)
  return $ BoolLit b

-- Parses integer literals
intLit :: Parser (Loc Expr)
intLit = locate $ lexeme $ do
  n <- L.decimal
  return $ IntLit n

{- FOURMOLU_DISABLE -}
-- Operator table
-- Defines operator precedence and associativity
operatorTable :: [[Operator Parser (Loc Expr)]]
operatorTable =
  [
    [ Prefix (locateOp1  "~"  Not)
    , Prefix (locateOp1  "-"  Neg)
    ]
  , [ InfixL (locateOp2  "*"  Mul)]
  , [ InfixL (locateOp2  "+"  Add)
    , InfixL (locateOp2  "-"  Sub)
    ]
  , [ InfixN (locateOp2 "<=>" Iff)
    , InfixN (locateOp2  "==" Eq)
    , InfixN (locateOp2  "!=" Neq)
    , InfixN (locateOp2  "<=" Leq)
    , InfixN (locateOp2  ">=" Geq)
    , InfixN (locateOp2  "<"  Lt)
    , InfixN (locateOp2  ">"  Gt)
    ]
  , [ InfixL (locateOp2 "/\\" And)]
  , [ InfixL (locateOp2 "\\/" Or)]
  , [ InfixR (locateOp2  "=>" Implies)]
  ]
{- FOURMOLU_ENABLE -}

-- Parses terms (parenthesized expressions, literals, variables)
term :: Parser (Loc Expr)
term =
  choice
    [ parens expr
    , boolLit
    , intLit
    , var
    ]

-- Parses expressions using the operator table
expr :: Parser (Loc Expr)
expr = makeExprParser term operatorTable

-- Statements

-- Parses variable declarations
declare :: Parser (Loc Statement)
declare = locate $ lexeme $ do
  keyword "var"
  v <- (locate $ identifier) `sepBy` symbol ","
  _ <- symbol ":"
  t <- bool <|> int
  return $ Declare v t

-- Parses variable assignments
assign :: Parser (Loc Statement)
assign = locate $ lexeme $ do
  keyword "let"
  v <- locate $ identifier
  _ <- symbol "="
  e <- expr
  return $ Assign v e

-- Parses assertions
assert :: Parser (Loc Statement)
assert = locate $ lexeme $ do
  keyword "assert"
  e <- expr
  return $ Assert e

-- Parses any statement
statement :: Parser (Loc Statement)
statement = declare <|> assign <|> assert

-- Entry point

program :: Parser Program
program = do
  sc
  statements <- many statement
  eof
  return $ Program statements

parseProgram :: String -> String -> Either (ParseErrorBundle Input Error) Program
parseProgram path content = parse program path content
