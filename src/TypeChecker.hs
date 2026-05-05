{-# LANGUAGE GADTs #-}

module TypeChecker (checkProgram) where

import Control.Monad
import qualified Data.Map as M

import AST
import Text.Megaparsec (SourcePos)

type Env = M.Map Identifier TType

data TypeError
  = UnboundVar SourcePos Identifier
  | DuplicateIdentifier SourcePos Identifier
  | TypeMismatch SourcePos TType TType
  deriving (Show)

checkExpr :: Env -> LExpr -> Either TypeError TType
checkExpr env (Located pos expr) = case expr of
  Var v -> case M.lookup v env of
    Just v' -> Right v'
    Nothing -> Left $ UnboundVar pos v
  BoolLit _ -> Right TBool
  IntLit _ -> Right TInt
  Not p -> do
    p' <- checkExpr env (Located pos p)
    case p' of
      TBool -> Right TBool
      _ -> Left $ TypeMismatch pos TBool p'
  And p q -> do
    p' <- checkExpr env (Located pos p)
    q' <- checkExpr env (Located pos q)
    case (p', q') of
      (TBool, TBool) -> Right TBool
      _ -> Left $ TypeMismatch pos TBool TBool
  Or p q -> do
    p' <- checkExpr env (Located pos p)
    q' <- checkExpr env (Located pos q)
    case (p', q') of
      (TBool, TBool) -> Right TBool
      _ -> Left $ TypeMismatch pos TBool TBool
  Implies p q -> do
    p' <- checkExpr env (Located pos p)
    q' <- checkExpr env (Located pos q)
    case (p', q') of
      (TBool, TBool) -> Right TBool
      _ -> Left $ TypeMismatch pos TBool TBool
  Iff p q -> do
    p' <- checkExpr env (Located pos p)
    q' <- checkExpr env (Located pos q)
    case (p', q') of
      (TBool, TBool) -> Right TBool
      _ -> Left $ TypeMismatch pos TBool TBool
  Neg m -> do
    m' <- checkExpr env (Located pos m)
    case m' of
      TInt -> Right TInt
      _ -> Left $ TypeMismatch pos TInt m'
  Add m n -> do
    m' <- checkExpr env (Located pos m)
    n' <- checkExpr env (Located pos n)
    case (m', n') of
      (TInt, TInt) -> Right TInt
      _ -> Left $ TypeMismatch pos TInt TInt
  Sub m n -> do
    m' <- checkExpr env (Located pos m)
    n' <- checkExpr env (Located pos n)
    case (m', n') of
      (TInt, TInt) -> Right TInt
      _ -> Left $ TypeMismatch pos TInt TInt
  Mul m n -> do
    m' <- checkExpr env (Located pos m)
    n' <- checkExpr env (Located pos n)
    case (m', n') of
      (TInt, TInt) -> Right TInt
      _ -> Left $ TypeMismatch pos TInt TInt
  Eq m n -> do
    m' <- checkExpr env (Located pos m)
    n' <- checkExpr env (Located pos n)
    case (m', n') of
      (TInt, TInt) -> Right TBool
      _ -> Left $ TypeMismatch pos TBool TBool
  Neq m n -> do
    m' <- checkExpr env (Located pos m)
    n' <- checkExpr env (Located pos n)
    case (m', n') of
      (TInt, TInt) -> Right TBool
      _ -> Left $ TypeMismatch pos TBool TBool
  Lt m n -> do
    m' <- checkExpr env (Located pos m)
    n' <- checkExpr env (Located pos n)
    case (m', n') of
      (TInt, TInt) -> Right TBool
      _ -> Left $ TypeMismatch pos TBool TBool
  Gt m n -> do
    m' <- checkExpr env (Located pos m)
    n' <- checkExpr env (Located pos n)
    case (m', n') of
      (TInt, TInt) -> Right TBool
      _ -> Left $ TypeMismatch pos TBool TBool
  Leq m n -> do
    m' <- checkExpr env (Located pos m)
    n' <- checkExpr env (Located pos n)
    case (m', n') of
      (TInt, TInt) -> Right TBool
      _ -> Left $ TypeMismatch pos TBool TBool
  Geq m n -> do
    m' <- checkExpr env (Located pos m)
    n' <- checkExpr env (Located pos n)
    case (m', n') of
      (TInt, TInt) -> Right TBool
      _ -> Left $ TypeMismatch pos TBool TBool

checkStatement :: Env -> LStatement -> Either TypeError Env
checkStatement env (Located pos stmt) = case stmt of
  Declare v t -> case M.member v env of
    True -> Left $ DuplicateIdentifier pos v
    False -> Right $ M.insert v t env
  Assign v e -> do
    t <- checkExpr env e
    return $ M.insert v t env
  Assert e -> do
    t <- checkExpr env e
    case t of
      TBool -> Right env
      _ -> Left $ TypeMismatch pos TBool t

checkProgram :: Program -> Either TypeError Program
checkProgram (Program stmts) = do
  _ <- foldM checkStatement M.empty stmts
  return $ Program stmts
