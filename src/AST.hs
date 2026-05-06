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

import qualified Data.List as L
import Text.Megaparsec

type Identifier = String

data Expr
  = -- Variables
    Var Identifier
  | -- Literals
    BoolLit Bool
  | IntLit Int
  | -- Logical operators
    Not LExpr
  | And LExpr LExpr
  | Or LExpr LExpr
  | Implies LExpr LExpr
  | Iff LExpr LExpr
  | -- Arithmetic operators
    Neg LExpr
  | Add LExpr LExpr
  | Sub LExpr LExpr
  | Mul LExpr LExpr
  | -- Comparison operators
    Eq LExpr LExpr
  | Neq LExpr LExpr
  | Lt LExpr LExpr
  | Gt LExpr LExpr
  | Leq LExpr LExpr
  | Geq LExpr LExpr

instance Show Expr where
  show (Var v) = show v
  show (BoolLit b) = show b
  show (IntLit n) = show n
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

data TType = TBool | TInt
  deriving (Eq)

instance Show TType where
  show TBool = "Bool"
  show TInt = "Int"

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

instance Show Statement where
  show (Declare v t) = "Declare " ++ v ++ " " ++ show t
  show (Assign v e) = "Assign " ++ v ++ " " ++ show e
  show (Assert e) = "Assert " ++ show e

type LStatement = Located Statement

data Program = Program [LStatement]

instance Show Program where
  show (Program stmts) = L.intercalate "\n" $ map show stmts
