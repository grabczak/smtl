{-# LANGUAGE GADTs #-}

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
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Keywords and identifiers

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

-- Locators

locate :: Parser a -> Parser (Located a)
locate p = Located <$> getSourcePos <*> p

locateUnary :: String -> (Expr -> Expr) -> Parser (LExpr -> LExpr)
locateUnary op constr = do
  _ <- symbol op
  return $ \e -> Located (loc e) (constr (node e))

locateBinary :: String -> (Expr -> Expr -> Expr) -> Parser (LExpr -> LExpr -> LExpr)
locateBinary op constr = do
  _ <- symbol op
  return $ \e1 e2 -> Located (loc e1) (constr (node e1) (node e2))

-- Expressions and statements

bool, int :: Parser TType
bool = lexeme $ keyword "bool" >> return TBool
int = lexeme $ keyword "int" >> return TInt

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
operatorTable :: [[Operator Parser LExpr]]
operatorTable =
  [ [ Prefix  (locateUnary "~"   Not)
    , Prefix  (locateUnary "-"   Neg)
    ]
  , [ InfixL  (locateBinary "*"   Mul) ]
  , [ InfixL  (locateBinary "+"   Add)
    , InfixL  (locateBinary "-"   Sub)
    ]
  , [ InfixN  (locateBinary "<=>" Iff)
    , InfixN  (locateBinary "==" Eq)
    , InfixN  (locateBinary "!=" Neq)
    , InfixN  (locateBinary "<=" Leq)
    , InfixN  (locateBinary ">=" Geq)
    , InfixN  (locateBinary "<"  Lt)
    , InfixN  (locateBinary ">"  Gt)
    ]
  , [ InfixL  (locateBinary "/\\" And) ]
  , [ InfixL  (locateBinary "\\/" Or)  ]
  , [ InfixR  (locateBinary "=>"  Implies) ] 
  ]
{- FOURMOLU_ENABLE -}

term :: Parser LExpr
term =
  choice
    [ parens expr
    , locate boolLit
    , locate intLit
    , locate var
    ]

expr :: Parser LExpr
expr = makeExprParser term operatorTable

declareVar :: Parser LStatement
declareVar = locate $ lexeme $ do
  keyword "var"
  v <- identifier
  _ <- symbol ":"
  t <- bool <|> int
  return $ Declare v t

letBinding :: Parser LStatement
letBinding = locate $ lexeme $ do
  keyword "let"
  v <- identifier
  _ <- symbol "="
  e <- expr
  return $ Assign v e

assertion :: Parser LStatement
assertion = locate $ lexeme $ do
  keyword "assert"
  e <- expr
  return $ Assert e

statement :: Parser LStatement
statement = do
  st <- declareVar <|> letBinding <|> assertion
  return st

-- Entry point

program :: Parser Program
program = do
  sc
  statements <- many statement
  eof
  return $ Program statements

parseProgram :: String -> String -> Either (ParseErrorBundle Input Error) Program
parseProgram path content = parse program path content
