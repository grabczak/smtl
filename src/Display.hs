module Display (typeErrorPretty) where

import Text.Megaparsec

import AST
import Data.Char

typeErrorPretty :: String -> TypeError -> String
typeErrorPretty file err = case err of
  UnboundVariable (SourcePos name line column) var ->
    formatError name line column (var ++ " is not defined")
  DuplicateIdentifier (SourcePos name line column) var ->
    formatError name line column (var ++ " is already defined")
  TypeMismatch (SourcePos name line column) expected actual ->
    formatError name line column ("expected " ++ toLowerCase expected ++ ", but got " ++ toLowerCase actual)
 where
  formatError name line column msg =
    let l = unPos line
        c = unPos column
        lineStr = show l
        content = getFileContent l
        pointer = makePointer c
        spacing = replicate (length lineStr + 1) ' '
     in unlines
          [ name ++ ":" ++ lineStr ++ ":" ++ show c ++ ":"
          , spacing ++ "|"
          , lineStr ++ " | " ++ content
          , spacing ++ "| " ++ pointer
          , msg
          ]

  getFileContent l =
    let fileLines = lines file
     in if l > 0 && l <= length fileLines then fileLines !! (l - 1) else ""

  makePointer c = replicate (max 0 (c - 1)) ' ' ++ "^"

  toLowerCase t = map toLower $ show t
