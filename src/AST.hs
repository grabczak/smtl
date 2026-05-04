{-# LANGUAGE GADTs #-}

module AST (
  Identifier,
  UType (..),
  UExpr (..),
  UStatement (..),
  UProgram (..),
  Type (..),
  Expr (..),
  AnyExpr (..),
  Statement (..),
  Program (..),
) where

type Identifier = String

-- Untyped AST, used for parsing

data UType = UBool | UInt
  deriving (Show)

data UExpr
  = UVar Identifier
  | UBoolLit Bool
  | UIntLit Int
  | UNot UExpr
  | UAnd UExpr UExpr
  | UOr UExpr UExpr
  | UImplies UExpr UExpr
  | UIff UExpr UExpr
  | UNeg UExpr
  | UAdd UExpr UExpr
  | USub UExpr UExpr
  | UMul UExpr UExpr
  | UEq UExpr UExpr
  | UNeq UExpr UExpr
  | ULt UExpr UExpr
  | UGt UExpr UExpr
  | ULeq UExpr UExpr
  | UGeq UExpr UExpr
  deriving (Show)

data UStatement
  = UDeclareVar Identifier UType
  | ULetBinding Identifier UExpr
  | UAssertion UExpr
  deriving (Show)

data UProgram = UProgram [UStatement]

instance Show UProgram where
  show (UProgram statements) = unlines $ map show statements

-- Typed AST, used for type checking

data Type = Bool | Int
  deriving (Show)

data Expr a where
  Var :: Identifier -> Expr a
  BoolLit :: Bool -> Expr Bool
  IntLit :: Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  And :: Expr Bool -> Expr Bool -> Expr Bool
  Or :: Expr Bool -> Expr Bool -> Expr Bool
  Implies :: Expr Bool -> Expr Bool -> Expr Bool
  Iff :: Expr Bool -> Expr Bool -> Expr Bool
  Neg :: Expr Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  Sub :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool
  Neq :: Expr Int -> Expr Int -> Expr Bool
  Lt :: Expr Int -> Expr Int -> Expr Bool
  Gt :: Expr Int -> Expr Int -> Expr Bool
  Leq :: Expr Int -> Expr Int -> Expr Bool
  Geq :: Expr Int -> Expr Int -> Expr Bool

instance Show (Expr a) where
  show (Var v) = v
  show (BoolLit b) = show b
  show (IntLit n) = show n
  show (Not p) = "not " ++ show p
  show (And p q) = "(" ++ show p ++ " and " ++ show q ++ ")"
  show (Or p q) = "(" ++ show p ++ " or " ++ show q ++ ")"
  show (Implies p q) = "(" ++ show p ++ " implies " ++ show q ++ ")"
  show (Iff p q) = "(" ++ show p ++ " iff " ++ show q ++ ")"
  show (Neg e) = "-(" ++ show e ++ ")"
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Eq e1 e2) = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
  show (Neq e1 e2) = "(" ++ show e1 ++ " != " ++ show e2 ++ ")"
  show (Lt e1 e2) = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
  show (Gt e1 e2) = "(" ++ show e1 ++ " > " ++ show e2 ++ ")"
  show (Leq e1 e2) = "(" ++ show e1 ++ " <= " ++ show e2 ++ ")"
  show (Geq e1 e2) = "(" ++ show e1 ++ " >= " ++ show e2 ++ ")"

data AnyExpr where
  AnyBool :: Expr Bool -> AnyExpr
  AnyInt :: Expr Int -> AnyExpr

instance Show AnyExpr where
  show (AnyBool e) = show e
  show (AnyInt e) = show e

data Statement
  = DeclareVar Identifier Type
  | LetBinding Identifier AnyExpr
  | Assertion (Expr Bool)
  deriving (Show)

data Program = Program [Statement]

instance Show Program where
  show (Program statements) = unlines $ map show statements
