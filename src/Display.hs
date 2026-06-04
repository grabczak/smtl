module Display (typeErrorPretty) where

import Text.Megaparsec

import AST
import Data.Char

typeErrorPretty :: String -> (Loc SemanticError) -> String
typeErrorPretty file (Loc start end err) = case err of
  UnboundVariable var ->
    formatError $ "unbound variable " ++ "'" ++ var ++ "'"
  DuplicateIdentifier var ->
    formatError $ "duplicate identifier " ++ "'" ++ var ++ "'"
  TypeMismatch expected actual ->
    formatError $ "expected " ++ lowercase expected ++ ", but got " ++ lowercase actual
 where
  lowercase x = map toLower (show x)
  formatError str =
    unlines
      [ sourceName start ++ ":" ++ show startPosLine ++ ":" ++ show startPosColumn
      , frontSpace ++ divider
      , show startPosLine ++ divider ++ fileLines !! (startPosLine - 1)
      , frontSpace ++ divider ++ replicate (startPosColumn - 1) ' ' ++ replicate (endPosColumn - startPosColumn) '^'
      , str
      , show start
      , show end
      ]
  startPosLine = unPos $ sourceLine start
  startPosColumn = unPos $ sourceColumn start
  endPosLine = unPos $ sourceLine end
  endPosColumn = unPos $ sourceColumn end
  endPosLineNumLength = length $ show endPosLine
  frontSpace = replicate endPosLineNumLength ' '
  fileLines = lines file
  divider = " | "
