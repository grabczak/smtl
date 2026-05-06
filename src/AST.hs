module AST (
  Located (..),
  Identifier,
  Type (..),
  Expr (..),
  Statement (..),
  Program (..),
  TypeError (..),
) where

import Text.Megaparsec

data Located a = Located
  { loc :: SourcePos
  , node :: a
  }

instance (Show a) => Show (Located a) where
  show (Located _ n) = show n

type Identifier = String

data Type = Bool | Int
  deriving (Show, Eq)

data Expr
  = -- Variables
    Var Identifier
  | -- Literals
    BoolLit Bool
  | IntLit Int
  | -- Logical operators
    Not (Located Expr)
  | And (Located Expr) (Located Expr)
  | Or (Located Expr) (Located Expr)
  | Implies (Located Expr) (Located Expr)
  | Iff (Located Expr) (Located Expr)
  | -- Arithmetic operators
    Neg (Located Expr)
  | Add (Located Expr) (Located Expr)
  | Sub (Located Expr) (Located Expr)
  | Mul (Located Expr) (Located Expr)
  | -- Comparison operators
    Eq (Located Expr) (Located Expr)
  | Neq (Located Expr) (Located Expr)
  | Lt (Located Expr) (Located Expr)
  | Gt (Located Expr) (Located Expr)
  | Leq (Located Expr) (Located Expr)
  | Geq (Located Expr) (Located Expr)

instance Show Expr where
  show (Var x) = x
  show (BoolLit b) = show b
  show (IntLit i) = show i
  show (Not e) = "(Not " ++ show (node e) ++ ")"
  show (And e1 e2) = "(And " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Or e1 e2) = "(Or " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Implies e1 e2) = "(Implies " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Iff e1 e2) = "(Iff " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Neg e) = "(Neg " ++ show (node e) ++ ")"
  show (Add e1 e2) = "(Add " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Sub e1 e2) = "(Sub " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Mul e1 e2) = "(Mul " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Eq e1 e2) = "(Eq " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Neq e1 e2) = "(Neq " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Lt e1 e2) = "(Lt " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Gt e1 e2) = "(Gt " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Leq e1 e2) = "(Leq " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"
  show (Geq e1 e2) = "(Geq " ++ show (node e1) ++ " " ++ show (node e2) ++ ")"

data Statement
  = Declare Identifier Type
  | Assign Identifier (Located Expr)
  | Assert (Located Expr)

instance Show Statement where
  show (Declare x t) = "Declare " ++ x ++ " " ++ show t
  show (Assign x e) = "Assign " ++ x ++ " " ++ show (node e)
  show (Assert e) = "Assert " ++ show (node e)

data Program = Program [Located Statement]

instance Show Program where
  show (Program statements) = unlines $ map show statements

data TypeError
  = UnboundVariable SourcePos Identifier
  | DuplicateIdentifier SourcePos Identifier
  | TypeMismatch SourcePos Type Type
