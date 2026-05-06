module Solver (smtlib) where

import qualified Data.List as L
import qualified Data.Map as M

import AST

-- Substitute assignments

type Env = M.Map Identifier (Located Expr)

-- Substitute an expression without location wrapper
substituteExpr' :: Env -> Expr -> Expr
substituteExpr' env expr = case expr of
  Var v -> case M.lookup v env of
    Just e -> node $ substituteExpr env e
    Nothing -> Var v
  BoolLit b -> BoolLit b
  IntLit n -> IntLit n
  Not e -> Not (go env e)
  And p q -> And (go env p) (go env q)
  Or p q -> Or (go env p) (go env q)
  Implies p q -> Implies (go env p) (go env q)
  Iff p q -> Iff (go env p) (go env q)
  Neg e -> Neg (go env e)
  Add m n -> Add (go env m) (go env n)
  Sub m n -> Sub (go env m) (go env n)
  Mul m n -> Mul (go env m) (go env n)
  Eq m n -> Eq (go env m) (go env n)
  Neq m n -> Neq (go env m) (go env n)
  Lt m n -> Lt (go env m) (go env n)
  Gt m n -> Gt (go env m) (go env n)
  Leq m n -> Leq (go env m) (go env n)
  Geq m n -> Geq (go env m) (go env n)
 where
  go = substituteExpr

-- Substitute a located expression
substituteExpr :: Env -> (Located Expr) -> Located Expr
substituteExpr env (Located pos expr) = Located pos (substituteExpr' env expr)

-- Substitute a statement without location wrapper
substituteStatement' :: Env -> Statement -> (Env, Statement)
substituteStatement' env statement = case statement of
  Declare v t -> (env, Declare v t)
  Assign v e -> (M.insert v e' env, Assign v (substituteExpr env e))
   where
    e' = substituteExpr env e
  Assert e -> (env, Assert (substituteExpr env e))

-- Substitute a located statement
substituteStatement :: Env -> (Located Statement) -> (Env, (Located Statement))
substituteStatement env (Located pos statement) = (env', Located pos statement')
 where
  (env', statement') = substituteStatement' env statement

-- Substitute entire program
substituteProgram :: Env -> Program -> Program
substituteProgram env (Program statements) = Program statements'
 where
  (_, statements') = L.mapAccumL substituteStatement env statements

-- Convert to SMT-LIB format

smtlibWrap :: String -> [String] -> String
smtlibWrap op args = "(" ++ op ++ " " ++ L.intercalate " " args ++ ")"

smtlibExpr :: (Located Expr) -> String
smtlibExpr (Located _ expr) = case expr of
  Var v -> v
  BoolLit b -> if b then "true" else "false"
  IntLit n -> show n
  Not e -> smtlibWrap "not" [smtlibExpr e]
  And e1 e2 -> smtlibWrap "and" [smtlibExpr e1, smtlibExpr e2]
  Or e1 e2 -> smtlibWrap "or" [smtlibExpr e1, smtlibExpr e2]
  Implies e1 e2 -> smtlibWrap "=>" [smtlibExpr e1, smtlibExpr e2]
  Iff e1 e2 -> smtlibWrap "=" [smtlibExpr e1, smtlibExpr e2]
  Neg e -> smtlibWrap "-" [smtlibExpr e]
  Add e1 e2 -> smtlibWrap "+" [smtlibExpr e1, smtlibExpr e2]
  Sub e1 e2 -> smtlibWrap "-" [smtlibExpr e1, smtlibExpr e2]
  Mul e1 e2 -> smtlibWrap "*" [smtlibExpr e1, smtlibExpr e2]
  Eq e1 e2 -> smtlibWrap "=" [smtlibExpr e1, smtlibExpr e2]
  Neq e1 e2 -> smtlibWrap "not" [smtlibWrap "=" [smtlibExpr e1, smtlibExpr e2]]
  Lt e1 e2 -> smtlibWrap "<" [smtlibExpr e1, smtlibExpr e2]
  Gt e1 e2 -> smtlibWrap ">" [smtlibExpr e1, smtlibExpr e2]
  Leq e1 e2 -> smtlibWrap "<=" [smtlibExpr e1, smtlibExpr e2]
  Geq e1 e2 -> smtlibWrap ">=" [smtlibExpr e1, smtlibExpr e2]

smtlibStatement :: (Located Statement) -> String
smtlibStatement (Located _ statement) = case statement of
  Declare v t -> smtlibWrap "declare-const" [v, show t]
  Assert e -> smtlibWrap "assert" [smtlibExpr e]
  _ -> ""

smtlib :: Program -> String
smtlib program =
  unlines $
    ["(set-logic QF_UFLIA)"]
      ++ filter (not . null) (map smtlibStatement statements)
      ++ ["(check-sat)", "(get-model)", "(exit)"]
 where
  Program statements = substituteProgram M.empty program
