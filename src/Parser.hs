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
identifier = do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_')
  let name = first : rest
  if name `elem` reserved
    then fail $ "Keyword '" ++ name ++ "' cannot be used as an identifier"
    else return name

-- Locators

-- Wraps a parser to include source position and span length
locate :: Parser a -> Parser (Loc a)
locate par = do
  startPos <- getSourcePos
  input <- stateInput <$> getParserState
  res <- par
  inputAfter <- stateInput <$> getParserState
  let spanLen = length input - length inputAfter
  return $ Loc startPos spanLen res

-- Puts a unary operator in a location wrapper
locateOp1 :: String -> (Loc Expr -> Expr) -> Parser (Loc Expr -> Loc Expr)
locateOp1 op constr = do
  _ <- symbol op
  return $ \e -> Loc (loc e) (len e) (constr e)

-- Puts a binary operator in a location wrapper
locateOp2 :: String -> (Loc Expr -> Loc Expr -> Expr) -> Parser (Loc Expr -> Loc Expr -> Loc Expr)
locateOp2 op constr = do
  _ <- symbol op
  return $ \e1 e2 ->
    let startPos = loc e1
        startCol = unPos (sourceColumn startPos)
        endCol = unPos (sourceColumn (loc e2)) + len e2
        totalLen = endCol - startCol
     in Loc startPos totalLen (constr e1 e2)

-- Expressions and statements

-- Parses type keywords
bool, int :: Parser Type
bool = lexeme $ keyword "bool" >> return Bool
int = lexeme $ keyword "int" >> return Int

-- Parses variable identifiers
var :: Parser (Loc Expr)
var = lexeme $ locate $ do
  v <- identifier
  return $ Var v

-- Parses boolean literals
boolLit :: Parser (Loc Expr)
boolLit = lexeme $ locate $ do
  b <- (keyword "T" >> return True) <|> (keyword "F" >> return False)
  return $ BoolLit b

-- Parses integer literals
intLit :: Parser (Loc Expr)
intLit = lexeme $ locate $ do
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
    [ parenExpr
    , intLit
    , boolLit
    , var
    ]

-- Parses a parenthesized expression with position at the opening paren
parenExpr :: Parser (Loc Expr)
parenExpr = do
  startPos <- getSourcePos
  let startCol = unPos (sourceColumn startPos)
  _ <- symbol "("
  e <- expr
  _ <- char ')'
  endPos <- getSourcePos
  space
  let endCol = unPos (sourceColumn endPos)
      spanLen = endCol - startCol
  return $ Loc startPos spanLen (node e)

-- Parses expressions using the operator table
expr :: Parser (Loc Expr)
expr = makeExprParser term operatorTable

-- Statements

-- Parses variable declarations
declare :: Parser (Loc Statement)
declare = locate $ lexeme $ do
  keyword "var"
  v <- (lexeme $ locate identifier) `sepBy` symbol ","
  _ <- symbol ":"
  t <- bool <|> int
  return $ Declare v t

-- Parses variable assignments
assign :: Parser (Loc Statement)
assign = locate $ lexeme $ do
  keyword "let"
  v <- lexeme $ locate identifier
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
