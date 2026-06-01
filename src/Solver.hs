{-# LANGUAGE GHC2024 #-}

module Solver (smtlib) where

import Control.Monad.State
import Data.List qualified as L
import Data.Map qualified as M

import AST

-- Substitution of variables with their assigned expressions

type Env = M.Map Identifier (Loc Expr)

substituteExpr :: Expr -> State Env Expr
substituteExpr = \case
  Var v -> do
    env <- get
    case M.lookup v env of
      Just e -> go e >>= return . node
      Nothing -> return $ Var v
  Lit l -> case l of
    LitBool b -> return $ Lit (LitBool b)
    LitInt n -> return $ Lit (LitInt n)
  Not p -> Not <$> go p
  And p q -> And <$> go p <*> go q
  Or p q -> Or <$> go p <*> go q
  Imp p q -> Imp <$> go p <*> go q
  Iff p q -> Iff <$> go p <*> go q
  Neg m -> Neg <$> go m
  Add m n -> Add <$> go m <*> go n
  Sub m n -> Sub <$> go m <*> go n
  Mul m n -> Mul <$> go m <*> go n
  Eq m n -> Eq <$> go m <*> go n
  Neq m n -> Neq <$> go m <*> go n
  Lt m n -> Lt <$> go m <*> go n
  Gt m n -> Gt <$> go m <*> go n
  Leq m n -> Leq <$> go m <*> go n
  Geq m n -> Geq <$> go m <*> go n
 where
  go = substituteLocExpr

substituteLocExpr :: (Loc Expr) -> State Env (Loc Expr)
substituteLocExpr (Loc startPos endPos expr) = substituteExpr expr >>= return . Loc startPos endPos

substituteStatement :: Statement -> State Env Statement
substituteStatement statement = case statement of
  Declare vs s -> return $ Declare vs s
  Assign (Loc startPos endPos v) e -> do
    e' <- substituteLocExpr e
    modify $ M.insert v e'
    return $ Assign (Loc startPos endPos v) e'
  Assert e -> do
    e' <- substituteLocExpr e
    return $ Assert e'

substituteLocStatement :: (Loc Statement) -> State Env (Loc Statement)
substituteLocStatement (Loc startPos endPos statement) = substituteStatement statement >>= return . Loc startPos endPos

substituteProgram :: Program -> Program
substituteProgram (Program statements) = Program $ evalState go M.empty
 where
  go = mapM substituteLocStatement statements

-- Conversion to SMT-LIB format

smtlibWrap :: String -> [String] -> String
smtlibWrap op args = "(" ++ op ++ " " ++ L.intercalate " " args ++ ")"

smtlibExpr :: (Loc Expr) -> String
smtlibExpr (Loc _ _ expr) = case expr of
  Var v -> v
  Lit l -> case l of
    LitBool b -> if b then "true" else "false"
    LitInt n -> show n
  Not e -> smtlibWrap "not" [smtlibExpr e]
  And e1 e2 -> smtlibWrap "and" [smtlibExpr e1, smtlibExpr e2]
  Or e1 e2 -> smtlibWrap "or" [smtlibExpr e1, smtlibExpr e2]
  Imp e1 e2 -> smtlibWrap "=>" [smtlibExpr e1, smtlibExpr e2]
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

smtlibStatement :: (Loc Statement) -> String
smtlibStatement (Loc _ _ statement) = case statement of
  Declare vs s -> L.intercalate "\n" $ map (\(Loc _ _ v) -> smtlibWrap "declare-const" [v, show s]) vs
  Assert e -> smtlibWrap "assert" [smtlibExpr e]
  _ -> ""

setLogic, checkSat, getModel, exit :: String
setLogic = "(set-logic QF_NIA)"
checkSat = "(check-sat)"
getModel = "(get-model)"
exit = "(exit)"

smtlib :: Program -> String
smtlib program =
  unlines $
    [setLogic]
      ++ filter (not . null) (map smtlibStatement statements)
      ++ [checkSat, getModel, exit]
 where
  Program statements = substituteProgram program
