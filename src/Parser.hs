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

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Keywords and identifiers

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

keyword :: String -> Parser ()
keyword kw = lexeme $ do
  _ <- string kw
  notFollowedBy alphaNumChar

identifier :: Parser Identifier
identifier = lexeme $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_')
  let name = first : rest
  if name `elem` reserved
    then fail $ "Keyword '" ++ name ++ "' cannot be used as an identifier"
    else return name

-- Locators

locate :: Parser a -> Parser (Located a)
locate p = Located <$> getSourcePos <*> p

locateOp1 :: String -> (Located Expr -> Expr) -> Parser (Located Expr -> Located Expr)
locateOp1 op constr = do
  pos <- getSourcePos
  _ <- symbol op
  return $ \e -> Located pos (constr e)

locateOp2 :: String -> (Located Expr -> Located Expr -> Expr) -> Parser (Located Expr -> Located Expr -> Located Expr)
locateOp2 op constr = do
  pos <- getSourcePos
  _ <- symbol op
  return $ \e1 e2 -> Located pos (constr e1 e2)

-- Expressions and statements

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

{- FOURMOLU_DISABLE -}
operatorTable :: [[Operator Parser (Located Expr)]]
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

term :: Parser (Located Expr)
term =
  choice
    [ parens expr
    , locate boolLit
    , locate intLit
    , locate var
    ]

expr :: Parser (Located Expr)
expr = makeExprParser term operatorTable

-- Statements

declare :: Parser (Located Statement)
declare = locate $ lexeme $ do
  keyword "var"
  v <- identifier
  _ <- symbol ":"
  t <- bool <|> int
  return $ Declare v t

assign :: Parser (Located Statement)
assign = locate $ lexeme $ do
  keyword "let"
  v <- identifier
  _ <- symbol "="
  e <- expr
  return $ Assign v e

assert :: Parser (Located Statement)
assert = locate $ lexeme $ do
  keyword "assert"
  e <- expr
  return $ Assert e

statement :: Parser (Located Statement)
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
