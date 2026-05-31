module Display (typeErrorPretty) where

import Text.Megaparsec

import AST
import Data.Char
import Data.List ((!?))

typeErrorPretty :: String -> (Loc TypeError) -> String
typeErrorPretty file (Loc startPos spanLen err) = case err of
  UnboundVariable var ->
    formatError $ "unbound variable " ++ "'" ++ var ++ "'"
  DuplicateIdentifier var ->
    formatError $ "duplicate identifier " ++ "'" ++ var ++ "'"
  TypeMismatch expected actual ->
    formatError $ "expected " ++ lowercase expected ++ ", but got " ++ lowercase actual
 where
  lowercase x = map toLower (show x)
  l = unPos (sourceLine startPos)
  c = unPos (sourceColumn startPos)
  lineStr = show l
  content = maybe "" id (lines file !? (l - 1))
  pointer = replicate (max 0 (c - 1)) ' ' ++ replicate spanLen '~'
  spacing = replicate (length lineStr + 1) ' '
  formatError msg =
    unlines
      [ sourceName startPos ++ ":" ++ lineStr ++ ":" ++ show c ++ ":"
      , spacing ++ "|"
      , lineStr ++ " | " ++ content
      , spacing ++ "| " ++ pointer
      , msg
      ]
