module TypeChecker (checkProgram) where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

import AST

-- Environment for type checking
type Env = M.Map Identifier Type

-- Checks a unary operation
checkOp1 :: (Loc Expr) -> Type -> Type -> StateT Env (Either (Loc TypeError)) Type
checkOp1 expr expected result = do
  actual <- checkExpr expr
  if actual == expected
    then lift $ Right $ result
    else lift $ Left $ Loc (loc expr) (len expr) $ TypeMismatch expected actual

-- Checks a binary operation
checkOp2 :: (Loc Expr) -> (Loc Expr) -> Type -> Type -> StateT Env (Either (Loc TypeError)) Type
checkOp2 expr1 expr2 expected result = do
  actual1 <- checkExpr expr1
  actual2 <- checkExpr expr2
  if actual1 == expected && actual2 == expected
    then lift $ Right result
    else
      if actual1 /= expected
        then lift $ Left $ Loc (loc expr1) (len expr1) $ TypeMismatch expected actual1
        else lift $ Left $ Loc (loc expr2) (len expr2) $ TypeMismatch expected actual2

-- Checks an expression and returns its type
checkExpr :: (Loc Expr) -> StateT Env (Either (Loc TypeError)) Type
checkExpr (Loc pos spanLen expr) = do
  env <- get
  case expr of
    Var v -> do
      case M.lookup v env of
        (Just t) -> return t
        Nothing -> lift $ Left $ Loc pos spanLen $ UnboundVariable v
    BoolLit _ -> return Bool
    IntLit _ -> return Int
    Not p -> checkOp1 p Bool Bool
    And p q -> checkOp2 p q Bool Bool
    Or p q -> checkOp2 p q Bool Bool
    Implies p q -> checkOp2 p q Bool Bool
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

-- Checks a statement and updates the environment
checkStatement :: (Loc Statement) -> StateT Env (Either (Loc TypeError)) ()
checkStatement (Loc _ _ statement) = do
  env <- get
  case statement of
    Declare vs t -> do
      env' <- lift $ foldM go env vs
      put env'
     where
      go acc (Loc pos spanLen v)
        | M.member v acc = Left $ Loc pos spanLen $ DuplicateIdentifier v
        | otherwise = Right $ M.insert v t acc
    Assign (Loc pos spanLen v) expr
      | M.member v env -> lift $ Left $ Loc pos spanLen $ DuplicateIdentifier v
      | otherwise -> do
          t <- checkExpr expr
          put $ M.insert v t env
    Assert (Loc pos spanLen v) -> do
      t <- checkExpr (Loc pos spanLen v)
      when (t /= Bool) (lift $ Left $ Loc pos spanLen $ TypeMismatch Bool t)

-- Checks entire program
checkProgram :: Program -> Either (Loc TypeError) Program
checkProgram (Program statements) = case evalStateT (mapM_ checkStatement statements) M.empty of
  Right _ -> return $ Program statements
  Left e -> Left e
