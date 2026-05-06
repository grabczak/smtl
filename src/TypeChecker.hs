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
     in name ++ ":" ++ lineStr ++ ":" ++ show c ++ ":\n" ++ spacing ++ "|\n" ++ lineStr ++ " | " ++ content ++ "\n" ++ spacing ++ "| " ++ pointer ++ "\n" ++ msg

type Env = M.Map Identifier TType

expectUnary :: Env -> LExpr -> TType -> TType -> Either TypeError TType
expectUnary env e expected result = do
  e' <- checkExpr env e
  case e' of
    e'' | e'' == expected -> Right result
    _ -> Left $ TypeMismatch (loc e) expected e'

expectBinary :: Env -> LExpr -> LExpr -> TType -> TType -> Either TypeError TType
expectBinary env e f expected result = do
  e' <- checkExpr env e
  f' <- checkExpr env f
  case (e', f') of
    (e'', f'') | e'' == expected && f'' == expected -> Right result
    (e'', _) | e'' /= expected -> Left $ TypeMismatch (loc e) expected f'
    _ -> Left $ TypeMismatch (loc f) expected f'

checkExpr :: Env -> LExpr -> Either TypeError TType
checkExpr env (Located pos expr) = case expr of
  Var v -> case M.lookup v env of
    Just v' -> Right v'
    Nothing -> Left $ UnboundVariable pos v
  BoolLit _ -> Right TBool
  IntLit _ -> Right TInt
  Not p -> expectUnary env p TBool TBool
  And p q -> expectBinary env p q TBool TBool
  Or p q -> expectBinary env p q TBool TBool
  Implies p q -> expectBinary env p q TBool TBool
  Iff p q -> expectBinary env p q TBool TBool
  Neg m -> expectUnary env m TInt TInt
  Add m n -> expectBinary env m n TInt TInt
  Sub m n -> expectBinary env m n TInt TInt
  Mul m n -> expectBinary env m n TInt TInt
  Eq m n -> expectBinary env m n TInt TBool
  Neq m n -> expectBinary env m n TInt TBool
  Lt m n -> expectBinary env m n TInt TBool
  Gt m n -> expectBinary env m n TInt TBool
  Leq m n -> expectBinary env m n TInt TBool
  Geq m n -> expectBinary env m n TInt TBool

checkStatement :: Env -> LStatement -> Either TypeError Env
checkStatement env (Located pos stmt) = case stmt of
  Declare v t -> case M.member v env of
    True -> Left $ DuplicateIdentifier pos v
    False -> Right $ M.insert v t env
  Assign v e -> case M.member v env of
    True -> Left $ DuplicateIdentifier pos v
    False -> do
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
