module Display (typeErrorPretty) where

import Text.Megaparsec

import AST
import Data.Char
import Data.List ((!?))

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

  startLine = unPos (sourceLine start)
  startCol = unPos (sourceColumn start)
  endLine = unPos (sourceLine end)
  endCol = unPos (sourceColumn end)
  lineStr = show startLine
  spacing = replicate (length lineStr + 1) ' '

  formatError msg =
    unlines $
      [ sourceName start ++ ":" ++ lineStr ++ ":" ++ show startCol ++ ":"
      , spacing ++ "|"
      ]
        ++ getErrorLines
        ++ [msg]

  getErrorLines = concat [formatErrorLine line | line <- [startLine .. endLine]]

  formatErrorLine lineNum =
    let content = maybe "" id (lines file !? (lineNum - 1))
        numStr = show lineNum
        underline = mkUnderline lineNum (length content)
     in [numStr ++ " | " ++ content, replicate (length numStr + 1) ' ' ++ "| " ++ underline]

  mkUnderline lineNum lineLen =
    let ulStart = if lineNum == startLine then max 0 (startCol - 1) else 0
        ulEnd = if lineNum == endLine then endCol else lineLen + 1
        len = max 1 (ulEnd - ulStart)
     in replicate ulStart ' ' ++ replicate len '^'
