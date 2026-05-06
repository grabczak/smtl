module Solver (smtlib) where

import qualified Data.List as L
import qualified Data.Map as M

import AST

type Env = M.Map Identifier LExpr

substitute :: Env -> LExpr -> LExpr
substitute env (Located pos expr) = case expr of
  Var v -> case M.lookup v env of
    Just e -> substitute env e
    Nothing -> Located pos (Var v)
  BoolLit b -> Located pos (BoolLit b)
  IntLit n -> Located pos (IntLit n)
  Not e -> Located pos (Not (substitute env e))
  And e1 e2 -> Located pos (And (substitute env e1) (substitute env e2))
  Or e1 e2 -> Located pos (Or (substitute env e1) (substitute env e2))
  Implies e1 e2 -> Located pos (Implies (substitute env e1) (substitute env e2))
  Iff e1 e2 -> Located pos (Iff (substitute env e1) (substitute env e2))
  Neg e -> Located pos (Neg (substitute env e))
  Add e1 e2 -> Located pos (Add (substitute env e1) (substitute env e2))
  Sub e1 e2 -> Located pos (Sub (substitute env e1) (substitute env e2))
  Mul e1 e2 -> Located pos (Mul (substitute env e1) (substitute env e2))
  Eq e1 e2 -> Located pos (Eq (substitute env e1) (substitute env e2))
  Neq e1 e2 -> Located pos (Neq (substitute env e1) (substitute env e2))
  Lt e1 e2 -> Located pos (Lt (substitute env e1) (substitute env e2))
  Gt e1 e2 -> Located pos (Gt (substitute env e1) (substitute env e2))
  Leq e1 e2 -> Located pos (Leq (substitute env e1) (substitute env e2))
  Geq e1 e2 -> Located pos (Geq (substitute env e1) (substitute env e2))

substituteStatement :: Env -> LStatement -> (Env, LStatement)
substituteStatement env (Located pos stmt) = case stmt of
  Declare v t -> (env, Located pos (Declare v t))
  Assign v e -> (M.insert v e' env, Located pos (Assign v e'))
   where
    e' = substitute env e
  Assert e -> (env, Located pos (Assert e'))
   where
    e' = substitute env e

substituteProgram :: Env -> Program -> (Env, Program)
substituteProgram env (Program stmts) = let (env', stmts') = foldl go (env, []) stmts in (env', Program stmts')
 where
  go (env', stmts') stmt = let (env'', stmt') = substituteStatement env' stmt in (env'', stmts' ++ [stmt'])

smtlib :: Program -> String
smtlib program = "(set-logic QF_UFLIA)\n" ++ (L.intercalate "\n" stmts') ++ "\n(check-sat)\n(get-model)\n(exit)"
 where
  (_, Program stmts) = substituteProgram M.empty program
  smtLibExpr (Located _ expr) = case expr of
    Var v -> v
    BoolLit b -> if b then "true" else "false"
    IntLit n -> show n
    Not e -> "(not " ++ smtLibExpr e ++ ")"
    And e1 e2 -> "(and " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Or e1 e2 -> "(or " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Implies e1 e2 -> "(=> " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Iff e1 e2 -> "(= " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Neg e -> "(- " ++ smtLibExpr e ++ ")"
    Add e1 e2 -> "(+ " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Sub e1 e2 -> "(- " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Mul e1 e2 -> "(* " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Eq e1 e2 -> "(= " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Neq e1 e2 -> "(not (= " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ "))"
    Lt e1 e2 -> "(< " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Gt e1 e2 -> "(> " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Leq e1 e2 -> "(<= " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
    Geq e1 e2 -> "(>= " ++ smtLibExpr e1 ++ " " ++ smtLibExpr e2 ++ ")"
  smtLibStatement (Located _ stmt) = case stmt of
    Declare v t -> "(declare-const " ++ v ++ " " ++ show t ++ ")"
    Assert e -> "(assert " ++ smtLibExpr e ++ ")"
    _ -> ""
  stmts' = filter (not . null) $ map smtLibStatement stmts
