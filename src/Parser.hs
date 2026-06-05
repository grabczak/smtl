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

skipLineComment, skipBlockComment :: Parser ()
skipLineComment = L.skipLineComment "#"
skipBlockComment = empty

-- Space consumer
sc :: Parser ()
sc = L.space space1 skipLineComment skipBlockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Keywords and identifiers

reserved :: [String]
reserved =
  [ "set"
  , "logic"
  , "QF_LIA"
  , "QF_NIA"
  , "var"
  , "let"
  , "assert"
  , "bool"
  , "int"
  , "T"
  , "F"
  , "check"
  , "sat"
  , "get"
  , "model"
  , "exit"
  ]

keyword :: String -> Parser ()
keyword kw = do
  _ <- string kw
  notFollowedBy alphaNumChar

identifier :: Parser Identifier
identifier = do
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
  _ <- lexeme $ try $ string op
  return $ \e -> Loc (startPos e) (endPos e) (cons e)

locateOp2 :: String -> (Loc Expr -> Loc Expr -> Expr) -> Parser (Loc Expr -> Loc Expr -> Loc Expr)
locateOp2 op cons = do
  _ <- lexeme $ try $ sym op
  return $ \e f -> Loc (startPos e) (endPos f) (cons e f)
 where
  sym "<" = string "<" <* notFollowedBy (char '=')
  sym ">" = string ">" <* notFollowedBy (char '=')
  sym "<=" = string "<=" <* notFollowedBy (char '>')
  sym "==" = string "==" <* notFollowedBy (char '>')
  sym "/" = string "/" <* notFollowedBy (char '\\')
  sym s = string s

-- Expressions and statements

qflia, qfnia :: Parser (Loc Logic)
qflia = locate $ keyword "QF_LIA" >> return QF_LIA
qfnia = locate $ keyword "QF_NIA" >> return QF_NIA

logic :: Parser (Loc Logic)
logic = qflia <|> qfnia

bool, int :: Parser (Loc Sort)
bool = locate $ keyword "bool" >> return Bool
int = locate $ keyword "int" >> return Int

sort :: Parser (Loc Sort)
sort = bool <|> int

var :: Parser (Loc Expr)
var = locate $ do
  v <- identifier
  return $ Var v

true, false :: Parser Bool
true = char 'T' >> return True
false = char 'F' >> return False

lbool :: Parser (Loc Expr)
lbool = locate $ do
  b <- true <|> false
  return $ LBool b

lint :: Parser (Loc Expr)
lint = locate $ do
  n <- L.decimal
  return $ LInt n

lit :: Parser (Loc Expr)
lit = lbool <|> lint

parens :: Parser (Loc Expr)
parens = do
  start <- getSourcePos
  -- Closing paren must be a string so that SourcePos is correct
  e <- between (symbol "(") (string ")") expr
  end <- getSourcePos
  return $ Loc start end (node e)

{- FOURMOLU_DISABLE -}
operatorTable :: [[Operator Parser (Loc Expr)]]
operatorTable =
  [ 
    [ Prefix (locateOp1  "~"  Not)
    , Prefix (locateOp1  "-"  Neg)
    ]
  , [ InfixL (locateOp2  "*"  Mul)
    , InfixL (locateOp2  "/"  Div)
    , InfixL (locateOp2  "%"  Mod)
    ]
  , [ InfixL (locateOp2  "+"  Add)
    , InfixL (locateOp2  "-"  Sub)
    ]
  , [ InfixN (locateOp2  "==" Eq)
    , InfixN (locateOp2  "!=" Neq)
    , InfixN (locateOp2  "<=" Leq)
    , InfixN (locateOp2  ">=" Geq)
    , InfixN (locateOp2  "<"  Lt)
    , InfixN (locateOp2  ">"  Gt)
    ]
  , [ InfixL (locateOp2 "/\\" And)]
  , [ InfixL (locateOp2 "\\/" Or)]
  , [ InfixL (locateOp2 "<=>" Iff)]
  , [ InfixR (locateOp2  "=>" Imp)]
  ]
{- FOURMOLU_ENABLE -}

term :: Parser (Loc Expr)
term = lexeme $ parens <|> lit <|> var

expr :: Parser (Loc Expr)
expr = makeExprParser term operatorTable

-- Statements

setLogic :: Parser (Loc Statement)
setLogic = locate $ do
  lexeme $ keyword "set"
  lexeme $ keyword "logic"
  l <- logic
  return $ SetLogic l

declare :: Parser (Loc Statement)
declare = locate $ do
  lexeme $ keyword "var"
  vs <- lexeme $ (locate identifier) `sepBy` symbol ","
  _ <- symbol ":"
  s <- sort
  return $ Declare vs s

assign :: Parser (Loc Statement)
assign = locate $ do
  lexeme $ keyword "let"
  v <- lexeme $ locate identifier
  _ <- symbol "="
  e <- expr
  return $ Assign v e

assert :: Parser (Loc Statement)
assert = locate $ do
  lexeme $ keyword "assert"
  e <- expr
  return $ Assert e

checkSat :: Parser (Loc Statement)
checkSat = locate $ do
  lexeme $ keyword "check"
  lexeme $ keyword "sat"
  return CheckSat

getModel :: Parser (Loc Statement)
getModel = locate $ do
  lexeme $ keyword "get"
  lexeme $ keyword "model"
  return GetModel

exit :: Parser (Loc Statement)
exit = locate $ do
  lexeme $ keyword "exit"
  return Exit

statement :: Parser (Loc Statement)
statement = setLogic <|> declare <|> assign <|> assert <|> checkSat <|> getModel <|> exit

-- Entry point

program :: Parser Program
program = do
  sc
  statements <- many (L.nonIndented sc statement)
  eof
  return $ Program statements

parseProgram :: String -> String -> Either (ParseErrorBundle Input Error) Program
parseProgram path content = parse program path content
