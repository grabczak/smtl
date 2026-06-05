{-# LANGUAGE GHC2024 #-}

module AST (
  Loc (..),
  Identifier,
  Sort (..),
  Expr (..),
  Statement (..),
  Program (..),
  SemanticError (..),
) where

import Data.List qualified as L
import Text.Megaparsec

data Loc a = Loc
  { startPos :: SourcePos
  , endPos :: SourcePos
  , node :: a
  }

instance (Show a) => Show (Loc a) where
  show x = show $ node x

type Identifier = String

-- Sort, a.k.a. type, of expressions and variables
data Sort = Bool | Int
  deriving (Eq, Show)

data Expr
  = -- Variables
    Var Identifier
  | -- Literals
    LBool Bool
  | LInt Int
  | -- Logical operators
    Not (Loc Expr)
  | And (Loc Expr) (Loc Expr)
  | Or (Loc Expr) (Loc Expr)
  | Imp (Loc Expr) (Loc Expr)
  | Iff (Loc Expr) (Loc Expr)
  | -- Arithmetic operators
    Neg (Loc Expr)
  | Add (Loc Expr) (Loc Expr)
  | Sub (Loc Expr) (Loc Expr)
  | Mul (Loc Expr) (Loc Expr)
  | -- Comparison operators
    Eq (Loc Expr) (Loc Expr)
  | Neq (Loc Expr) (Loc Expr)
  | Lt (Loc Expr) (Loc Expr)
  | Gt (Loc Expr) (Loc Expr)
  | Leq (Loc Expr) (Loc Expr)
  | Geq (Loc Expr) (Loc Expr)

instance Show Expr where
  show = \case
    Var v -> v
    LBool b -> show b
    LInt i -> show i
    Not p -> wrap1 "Not" p
    And p q -> wrap2 "And" p q
    Or p q -> wrap2 "Or" p q
    Imp p q -> wrap2 "Imp" p q
    Iff p q -> wrap2 "Iff" p q
    Neg e -> wrap1 "Neg" e
    Add e f -> wrap2 "Add" e f
    Sub e f -> wrap2 "Sub" e f
    Mul e f -> wrap2 "Mul" e f
    Eq e f -> wrap2 "Eq" e f
    Neq e f -> wrap2 "Neq" e f
    Lt e f -> wrap2 "Lt" e f
    Gt e f -> wrap2 "Gt" e f
    Leq e f -> wrap2 "Leq" e f
    Geq e f -> wrap2 "Geq" e f
   where
    wrap1 cons e = "(" ++ cons ++ " " ++ show e ++ ")"
    wrap2 cons e f = "(" ++ cons ++ " " ++ show e ++ " " ++ show f ++ ")"

data Statement
  = Declare [Loc Identifier] (Loc Sort)
  | Assign (Loc Identifier) (Loc Expr)
  | Assert (Loc Expr)

instance Show Statement where
  show = \case
    Declare vs t -> "Declare [" ++ L.intercalate ", " (map node vs) ++ "] " ++ show t
    Assign v e -> "Assign " ++ node v ++ " " ++ show e
    Assert e -> "Assert " ++ show e

newtype Program = Program [Loc Statement]

instance Show Program where
  show (Program statements) = unlines $ map show statements

data SemanticError
  = UnboundVariable Identifier
  | DuplicateIdentifier Identifier
  | TypeMismatch Sort Sort
