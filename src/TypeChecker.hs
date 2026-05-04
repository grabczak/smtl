{-# LANGUAGE GADTs #-}

module TypeChecker (checkProgram) where

import Control.Monad
import qualified Data.Map as M

import AST

type Env = M.Map Identifier UType

data TypeError
  = UnboundVar Identifier
  | DuplicateVar Identifier
  | TypeMismatch UType UType UExpr
  deriving (Show)

typeOf :: AnyExpr -> UType
typeOf (AnyBool _) = UBool
typeOf (AnyInt _) = UInt

toType :: UType -> Type
toType UBool = Bool
toType UInt = Int

-- This is going to need some serious refactoring
checkExpr :: Env -> UExpr -> Either TypeError AnyExpr
checkExpr _ (UBoolLit p) = Right $ AnyBool (BoolLit p)
checkExpr _ (UIntLit m) = Right $ AnyInt (IntLit m)
checkExpr env (UVar v) =
  case M.lookup v env of
    Just UBool -> Right $ AnyBool (Var v)
    Just UInt -> Right $ AnyInt (Var v)
    Nothing -> Left $ UnboundVar v
checkExpr env (UNot p) = do
  p' <- checkExpr env p
  case p' of
    AnyBool p'' -> Right $ AnyBool (Not p'')
    _ -> Left $ TypeMismatch UBool (typeOf p') (UNot p)
checkExpr env (UAnd p q) = do
  p' <- checkExpr env p
  q' <- checkExpr env q
  case (p', q') of
    (AnyBool p'', AnyBool q'') -> Right $ AnyBool (And p'' q'')
    (AnyBool _, _) -> Left $ TypeMismatch UBool (typeOf q') (UAnd p q)
    (_, _) -> Left $ TypeMismatch UBool (typeOf p') (UAnd p q)
checkExpr env (UOr p q) = do
  p' <- checkExpr env p
  q' <- checkExpr env q
  case (p', q') of
    (AnyBool p'', AnyBool q'') -> Right $ AnyBool (Or p'' q'')
    (AnyBool _, _) -> Left $ TypeMismatch UBool (typeOf q') (UOr p q)
    (_, _) -> Left $ TypeMismatch UBool (typeOf p') (UOr p q)
checkExpr env (UImplies p q) = do
  p' <- checkExpr env p
  q' <- checkExpr env q
  case (p', q') of
    (AnyBool p'', AnyBool q'') -> Right $ AnyBool (Implies p'' q'')
    (AnyBool _, _) -> Left $ TypeMismatch UBool (typeOf q') (UImplies p q)
    (_, _) -> Left $ TypeMismatch UBool (typeOf p') (UImplies p q)
checkExpr env (UIff p q) = do
  p' <- checkExpr env p
  q' <- checkExpr env q
  case (p', q') of
    (AnyBool p'', AnyBool q'') -> Right $ AnyBool (Iff p'' q'')
    (AnyBool _, _) -> Left $ TypeMismatch UBool (typeOf q') (UIff p q)
    (_, _) -> Left $ TypeMismatch UBool (typeOf p') (UIff p q)
checkExpr env (UNeg m) = do
  m' <- checkExpr env m
  case m' of
    (AnyInt m'') -> Right $ AnyInt (Neg m'')
    _ -> Left $ TypeMismatch UInt (typeOf m') (UNeg m)
checkExpr env (UAdd m n) = do
  m' <- checkExpr env m
  n' <- checkExpr env n
  case (m', n') of
    (AnyInt m'', AnyInt n'') -> Right $ AnyInt (Add m'' n'')
    (AnyInt _, _) -> Left $ TypeMismatch UInt (typeOf n') (UAdd m n)
    (_, _) -> Left $ TypeMismatch UInt (typeOf m') (UAdd m n)
checkExpr env (USub m n) = do
  m' <- checkExpr env m
  n' <- checkExpr env n
  case (m', n') of
    (AnyInt m'', AnyInt n'') -> Right $ AnyInt (Sub m'' n'')
    (AnyInt _, _) -> Left $ TypeMismatch UInt (typeOf n') (USub m n)
    (_, _) -> Left $ TypeMismatch UInt (typeOf m') (USub m n)
checkExpr env (UMul m n) = do
  m' <- checkExpr env m
  n' <- checkExpr env n
  case (m', n') of
    (AnyInt m'', AnyInt n'') -> Right $ AnyInt (Mul m'' n'')
    (AnyInt _, _) -> Left $ TypeMismatch UInt (typeOf n') (UMul m n)
    (_, _) -> Left $ TypeMismatch UInt (typeOf m') (UMul m n)
checkExpr env (UEq x y) = do
  x' <- checkExpr env x
  y' <- checkExpr env y
  case (x', y') of
    (AnyInt x'', AnyInt y'') -> Right $ AnyBool (Eq x'' y'')
    (AnyInt _, _) -> Left $ TypeMismatch UInt (typeOf y') (UEq x y)
    (_, _) -> Left $ TypeMismatch UInt (typeOf x') (UEq x y)
checkExpr env (UNeq x y) = do
  x' <- checkExpr env x
  y' <- checkExpr env y
  case (x', y') of
    (AnyInt x'', AnyInt y'') -> Right $ AnyBool (Neq x'' y'')
    (AnyInt _, _) -> Left $ TypeMismatch UInt (typeOf y') (UNeq x y)
    (_, _) -> Left $ TypeMismatch UInt (typeOf x') (UNeq x y)
checkExpr env (ULt x y) = do
  x' <- checkExpr env x
  y' <- checkExpr env y
  case (x', y') of
    (AnyInt x'', AnyInt y'') -> Right $ AnyBool (Lt x'' y'')
    (AnyInt _, _) -> Left $ TypeMismatch UInt (typeOf y') (ULt x y)
    (_, _) -> Left $ TypeMismatch UInt (typeOf x') (ULt x y)
checkExpr env (UGt x y) = do
  x' <- checkExpr env x
  y' <- checkExpr env y
  case (x', y') of
    (AnyInt x'', AnyInt y'') -> Right $ AnyBool (Gt x'' y'')
    (AnyInt _, _) -> Left $ TypeMismatch UInt (typeOf y') (UGt x y)
    (_, _) -> Left $ TypeMismatch UInt (typeOf x') (UGt x y)
checkExpr env (ULeq x y) = do
  x' <- checkExpr env x
  y' <- checkExpr env y
  case (x', y') of
    (AnyInt x'', AnyInt y'') -> Right $ AnyBool (Leq x'' y'')
    (AnyInt _, _) -> Left $ TypeMismatch UInt (typeOf y') (ULeq x y)
    (_, _) -> Left $ TypeMismatch UInt (typeOf x') (ULeq x y)
checkExpr env (UGeq x y) = do
  x' <- checkExpr env x
  y' <- checkExpr env y
  case (x', y') of
    (AnyInt x'', AnyInt y'') -> Right $ AnyBool (Geq x'' y'')
    (AnyInt _, _) -> Left $ TypeMismatch UInt (typeOf y') (UGeq x y)
    (_, _) -> Left $ TypeMismatch UInt (typeOf x') (UGeq x y)

checkStatement :: Env -> UStatement -> Either TypeError (Env, Statement)
checkStatement env (UDeclareVar v t) =
  case M.lookup v env of
    (Just _) -> Left $ DuplicateVar v
    Nothing -> Right $ (M.insert v t env, DeclareVar v (toType t))
checkStatement env (ULetBinding v e) = do
  e' <- checkExpr env e
  return (M.insert v (typeOf e') env, LetBinding v e')
checkStatement env (UAssertion e) = do
  e' <- checkExpr env e
  case e' of
    (AnyBool p) -> Right $ (env, Assertion p)
    _ -> Left $ TypeMismatch UBool (typeOf e') e

checkProgram :: UProgram -> Either TypeError Program
checkProgram (UProgram stmts) = do
  (_finalEnv, typedStatements) <- foldM step (M.empty, []) stmts
  return $ Program (reverse typedStatements)
 where
  step (env, acc) statement = do
    (env', statement') <- checkStatement env statement
    return (env', statement' : acc)
