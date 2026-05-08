{-# LANGUAGE GADTs #-}

module TypeChecker (checkProgram) where

import Control.Monad
import qualified Data.Map as M

import AST

type Env = M.Map Identifier Type

checkOp1 :: Env -> (Loc Expr) -> Type -> Type -> Either TypeError Type
checkOp1 env e expected result = do
  e' <- checkExpr env e
  if e' == expected
    then Right result
    else Left $ TypeMismatch (loc e) expected e'

checkOp2 :: Env -> (Loc Expr) -> (Loc Expr) -> Type -> Type -> Either TypeError Type
checkOp2 env e f expected result = do
  e' <- checkExpr env e
  f' <- checkExpr env f
  if e' == expected && f' == expected
    then Right result
    else
      if e' /= expected
        then Left $ TypeMismatch (loc e) expected e'
        else Left $ TypeMismatch (loc f) expected f'

checkExpr :: Env -> (Loc Expr) -> Either TypeError Type
checkExpr env (Loc pos expr) = case expr of
  Var v -> maybe (Left $ UnboundVariable pos v) Right (M.lookup v env)
  BoolLit _ -> Right Bool
  IntLit _ -> Right Int
  Not p -> checkOp1 env p Bool Bool
  And p q -> checkOp2 env p q Bool Bool
  Or p q -> checkOp2 env p q Bool Bool
  Implies p q -> checkOp2 env p q Bool Bool
  Iff p q -> checkOp2 env p q Bool Bool
  Neg m -> checkOp1 env m Int Int
  Add m n -> checkOp2 env m n Int Int
  Sub m n -> checkOp2 env m n Int Int
  Mul m n -> checkOp2 env m n Int Int
  Eq m n -> checkOp2 env m n Int Bool
  Neq m n -> checkOp2 env m n Int Bool
  Lt m n -> checkOp2 env m n Int Bool
  Gt m n -> checkOp2 env m n Int Bool
  Leq m n -> checkOp2 env m n Int Bool
  Geq m n -> checkOp2 env m n Int Bool

checkStatement :: Env -> (Statement) -> Either TypeError Env
checkStatement env statement = case statement of
  Declare vs t ->
    foldM
      ( \acc (Loc pos v) ->
          if M.member v acc
            then Left $ DuplicateIdentifier pos v
            else Right $ M.insert v t acc
      )
      env
      vs
  Assign (Loc pos v) e
    | M.member v env -> Left $ DuplicateIdentifier pos v
    | otherwise -> do
        t <- checkExpr env e
        Right $ M.insert v t env
  Assert (Loc pos e) -> do
    t <- checkExpr env (Loc pos e)
    if t == Bool
      then Right env
      else Left $ TypeMismatch pos Bool t

checkProgram :: Program -> Either TypeError Program
checkProgram (Program statements) = do
  _ <- foldM checkStatement M.empty statements
  return $ Program statements
