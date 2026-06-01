{-# LANGUAGE GHC2024 #-}

module Parser (parseProgram) where

import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

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

locate :: Parser a -> Parser (Loc a)
locate parser = do
  start <- getSourcePos
  result <- parser
  end <- getSourcePos
  return $ Loc start end result

locateOp1 :: String -> (Loc Expr -> Expr) -> Parser (Loc Expr -> Loc Expr)
locateOp1 op cons = do
  _ <- symbol op
  return $ \e -> Loc (startPos e) (endPos e) (cons e)

locateOp2 :: String -> (Loc Expr -> Loc Expr -> Expr) -> Parser (Loc Expr -> Loc Expr -> Loc Expr)
locateOp2 op cons = do
  _ <- symbol op
  return $ \e f -> Loc (startPos e) (endPos f) (cons e f)

-- Expressions and statements

bool, int :: Parser (Loc Sort)
bool = lexeme $ locate $ keyword "bool" >> return Bool
int = lexeme $ locate $ keyword "int" >> return Int

sort :: Parser (Loc Sort)
sort = bool <|> int

var :: Parser (Loc Expr)
var = lexeme $ locate $ do
  v <- identifier
  return $ Var v

litBool :: Parser (Loc Expr)
litBool = lexeme $ locate $ do
  b <- (keyword "T" >> return True) <|> (keyword "F" >> return False)
  return $ Lit (LitBool b)

litInt :: Parser (Loc Expr)
litInt = lexeme $ locate $ do
  n <- L.decimal
  return $ Lit (LitInt n)

lit :: Parser (Loc Expr)
lit = litBool <|> litInt

parens :: Parser (Loc Expr)
parens = lexeme $ do
  start <- getSourcePos
  e <- between (symbol "(") (symbol ")") expr
  end <- getSourcePos
  return $ Loc start end (node e)

{- FOURMOLU_DISABLE -}
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
  , [ InfixR (locateOp2  "=>" Imp)]
  ]
{- FOURMOLU_ENABLE -}

term :: Parser (Loc Expr)
term = parens <|> lit <|> var

expr :: Parser (Loc Expr)
expr = makeExprParser term operatorTable

-- Statements

declare :: Parser (Loc Statement)
declare = locate $ lexeme $ do
  keyword "var"
  v <- (locate identifier) `sepBy` symbol ","
  _ <- symbol ":"
  s <- sort
  return $ Declare v s

assign :: Parser (Loc Statement)
assign = locate $ lexeme $ do
  keyword "let"
  v <- locate identifier
  _ <- symbol "="
  e <- expr
  return $ Assign v e

assert :: Parser (Loc Statement)
assert = locate $ lexeme $ do
  keyword "assert"
  e <- expr
  return $ Assert e

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
