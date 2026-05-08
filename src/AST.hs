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
  | Assign Identifier (Loc Expr)
  | Assert (Loc Expr)

instance Show Statement where
  show (Declare x t) = "Declare " ++ x ++ " " ++ show t
  show (Assign x e) = "Assign " ++ x ++ " " ++ show (node e)
  show (Assert e) = "Assert " ++ show (node e)

data Program = Program [Loc Statement]

instance Show Program where
  show (Program statements) = unlines $ map show statements

data TypeError
  = UnboundVariable SourcePos Identifier
  | DuplicateIdentifier SourcePos Identifier
  | TypeMismatch SourcePos Type Type
