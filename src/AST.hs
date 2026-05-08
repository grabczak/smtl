module AST (
  Loc (..),
  Identifier,
  Type (..),
  Expr (..),
  Statement (..),
  Program (..),
  TypeError (..),
) where

import Text.Megaparsec

data Loc a = Loc
  { loc :: SourcePos
  , node :: a
  }

instance (Show a) => Show (Loc a) where
  show (Loc _ n) = show n

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
    Not (Loc Expr)
  | And (Loc Expr) (Loc Expr)
  | Or (Loc Expr) (Loc Expr)
  | Implies (Loc Expr) (Loc Expr)
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
  show (Var v) = v
  show (BoolLit b) = show b
  show (IntLit i) = show i
  show (Not e) = "(Not " ++ show e ++ ")"
  show (And e1 e2) = "(And " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Or e1 e2) = "(Or " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Implies e1 e2) = "(Implies " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Iff e1 e2) = "(Iff " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Neg e) = "(Neg " ++ show e ++ ")"
  show (Add e1 e2) = "(Add " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Sub e1 e2) = "(Sub " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Mul e1 e2) = "(Mul " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Eq e1 e2) = "(Eq " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Neq e1 e2) = "(Neq " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Lt e1 e2) = "(Lt " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Gt e1 e2) = "(Gt " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Leq e1 e2) = "(Leq " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Geq e1 e2) = "(Geq " ++ show e1 ++ " " ++ show e2 ++ ")"

data Statement
  = Declare [Loc Identifier] Type
  | Assign (Loc Identifier) (Loc Expr)
  | Assert (Loc Expr)

instance Show Statement where
  show (Declare vs t) = "Declare " ++ unwords (map node vs) ++ " " ++ show t
  show (Assign v e) = "Assign " ++ node v ++ " " ++ show e
  show (Assert e) = "Assert " ++ show e

data Program = Program [Statement]

instance Show Program where
  show (Program statements) = unlines $ map show statements

data TypeError
  = UnboundVariable SourcePos Identifier
  | DuplicateIdentifier SourcePos Identifier
  | TypeMismatch SourcePos Type Type
