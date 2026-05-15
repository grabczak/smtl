module Display (typeErrorPretty) where

import Text.Megaparsec

import AST
import Data.Char
import Data.List ((!?))

typeErrorPretty :: String -> (Loc TypeError) -> String
typeErrorPretty file (Loc ((SourcePos name line column)) err) = case err of
  UnboundVariable var ->
    formatError ("unbound variable " ++ "'" ++ var ++ "'")
  DuplicateIdentifier var ->
    formatError ("duplicate identifier " ++ "'" ++ var ++ "'")
  TypeMismatch expected actual ->
    formatError ("expected " ++ map toLower (show expected) ++ ", but got " ++ map toLower (show actual))
 where
  formatError msg =
    let l = unPos line
        c = unPos column
        lineStr = show l
        content = maybe "" id (lines file !? (l - 1))
        pointer = replicate (max 0 (c - 1)) ' ' ++ "^"
        spacing = replicate (length lineStr + 1) ' '
     in unlines
          [ name ++ ":" ++ lineStr ++ ":" ++ show c ++ ":"
          , spacing ++ "|"
          , lineStr ++ " | " ++ content
          , spacing ++ "| " ++ pointer
          , msg
          ]
