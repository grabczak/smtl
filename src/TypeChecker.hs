{-# LANGUAGE GADTs #-}

module TypeChecker (checkProgram, typeErrorPretty) where

import Control.Monad
import qualified Data.Map as M
import Text.Megaparsec

import AST

data TypeError
  = UnboundVariable SourcePos Identifier
  | DuplicateIdentifier SourcePos Identifier
  | TypeMismatch SourcePos TType TType
  deriving (Show)

typeErrorPretty :: String -> TypeError -> String
typeErrorPretty file err = case err of
  TypeMismatch (SourcePos name line column) expected actual ->
    go name line column ("expected " ++ show expected ++ ", but got " ++ show actual)
  UnboundVariable (SourcePos name line column) var ->
    go name line column (var ++ " is not defined")
  DuplicateIdentifier (SourcePos name line column) var ->
    go name line column (var ++ " is already defined")
 where
  go name line column msg =
    let l = unPos line
        c = unPos column
        fileLines = lines file
        content = if l > 0 && l <= length fileLines then fileLines !! (l - 1) else ""
        pointer = replicate (max 0 (c - 1)) ' ' ++ "^"
        lineStr = show l
        spacing = replicate (length lineStr + 1) ' '
    {- FOURMOLU_DISABLE -}
     in name ++ ":" ++ lineStr ++ ":" ++ show c ++ ":\n" ++
        spacing ++ "|\n" ++
        lineStr ++ " | " ++ content ++ "\n" ++
        spacing ++ "| " ++ pointer ++ "\n" ++
        msg
    {- FOURMOLU_ENABLE -}

type Env = M.Map Identifier TType

checkExpr :: Env -> LExpr -> Either TypeError TType
checkExpr env (Located pos expr) = case expr of
  Var v -> case M.lookup v env of
    Just v' -> Right v'
    Nothing -> Left $ UnboundVariable pos v
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
    Right $ M.insert v t env
  Assert e -> do
    t <- checkExpr env e
    case t of
      TBool -> Right env
      _ -> Left $ TypeMismatch pos TBool t

checkProgram :: Program -> Either TypeError Program
checkProgram (Program stmts) = do
  _ <- foldM checkStatement M.empty stmts
  return $ Program stmts
