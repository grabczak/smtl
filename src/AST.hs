module AST (
  Identifier,
  Expr (..),
  TType (..),
  Statement (..),
  Program (..),
  Located (..),
  LExpr,
  LStatement,
) where

import Text.Megaparsec (SourcePos)

type Identifier = String

data Expr
  = -- Variables
    Var Identifier
  | -- Literals
    BoolLit Bool
  | IntLit Int
  | -- Logical operators
    Not Expr
  | And Expr Expr
  | Or Expr Expr
  | Implies Expr Expr
  | Iff Expr Expr
  | -- Arithmetic operators
    Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | -- Comparison operators
    Eq Expr Expr
  | Neq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Leq Expr Expr
  | Geq Expr Expr
  deriving (Show)

data TType = TBool | TInt
  deriving (Show)

data Located a = Located
  { loc :: SourcePos
  , node :: a
  }

instance (Show a) => Show (Located a) where
  show l = show $ node l

type LExpr = Located Expr

data Statement
  = Declare Identifier TType
  | Assign Identifier LExpr
  | Assert LExpr
  deriving (Show)

type LStatement = Located Statement

data Program = Program [LStatement]

instance Show Program where
  show (Program stmts) = unlines $ map show stmts
