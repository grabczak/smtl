{-# LANGUAGE GHC2024 #-}

module TypeChecker (checkProgram) where

import Control.Monad
import Control.Monad.State
import Data.Map qualified as M

import AST

type Env = M.Map Identifier Sort

checkOp1 :: (Loc Expr) -> Sort -> Sort -> StateT Env (Either (Loc SemanticError)) Sort
checkOp1 expr expected result = do
  actual <- checkExpr expr
  lift $ case actual == expected of
    True -> Right result
    False -> Left $ Loc (startPos expr) (endPos expr) $ TypeMismatch expected actual

checkOp2 :: (Loc Expr) -> (Loc Expr) -> Sort -> Sort -> StateT Env (Either (Loc SemanticError)) Sort
checkOp2 expr1 expr2 expected result = do
  actual1 <- checkExpr expr1
  actual2 <- checkExpr expr2
  lift $ case (actual1 == expected, actual2 == expected) of
    (True, True) -> Right result
    (False, _) -> Left $ Loc (startPos expr1) (endPos expr1) $ TypeMismatch expected actual1
    (_, False) -> Left $ Loc (startPos expr2) (endPos expr2) $ TypeMismatch expected actual2

checkExpr :: (Loc Expr) -> StateT Env (Either (Loc SemanticError)) Sort
checkExpr (Loc startPos endPos expr) = case expr of
  Var v -> do
    env <- get
    case M.lookup v env of
      Just s -> return s
      Nothing -> lift $ Left $ Loc startPos endPos $ UnboundVariable v
  LBool _ -> return Bool
  LInt _ -> return Int
  Not p -> checkOp1 p Bool Bool
  And p q -> checkOp2 p q Bool Bool
  Or p q -> checkOp2 p q Bool Bool
  Imp p q -> checkOp2 p q Bool Bool
  Iff p q -> checkOp2 p q Bool Bool
  Neg m -> checkOp1 m Int Int
  Add m n -> checkOp2 m n Int Int
  Sub m n -> checkOp2 m n Int Int
  Mul m n -> checkOp2 m n Int Int
  Eq m n -> checkOp2 m n Int Bool
  Neq m n -> checkOp2 m n Int Bool
  Lt m n -> checkOp2 m n Int Bool
  Gt m n -> checkOp2 m n Int Bool
  Leq m n -> checkOp2 m n Int Bool
  Geq m n -> checkOp2 m n Int Bool

checkStatement :: (Loc Statement) -> StateT Env (Either (Loc SemanticError)) ()
checkStatement (Loc _ _ statement) = case statement of
  Declare vs s -> do
    env <- get
    env' <- lift $ foldM go env vs
    put env'
   where
    go acc (Loc startPos endPos v)
      | M.member v acc = Left $ Loc startPos endPos $ DuplicateIdentifier v
      | otherwise = Right $ M.insert v (node s) acc
  Assign (Loc startPos endPos v) expr -> do
    env <- get
    if M.member v env
      then lift $ Left $ Loc startPos endPos $ DuplicateIdentifier v
      else do
        s <- checkExpr expr
        modify $ M.insert v s
  Assert expr -> do
    s <- checkExpr expr
    when (s /= Bool) $ lift $ Left $ Loc (startPos expr) (endPos expr) $ TypeMismatch Bool s

checkProgram :: Program -> Either (Loc SemanticError) Program
checkProgram (Program statements) = case evalStateT go M.empty of
  Right _ -> return $ Program statements
  Left e -> Left e
 where
  go = mapM_ checkStatement statements
