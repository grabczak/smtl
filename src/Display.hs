{-# LANGUAGE GHC2024 #-}

module Display (semanticErrorPretty) where

import Data.Char qualified as C
import Data.List qualified as L
import Text.Megaparsec

import AST

semanticErrorPretty :: String -> (Loc SemanticError) -> String
semanticErrorPretty file (Loc start end err) = case err of
  UnboundVariable var ->
    formatError $ "unbound variable " ++ "'" ++ var ++ "'"
  DuplicateIdentifier var ->
    formatError $ "duplicate identifier " ++ "'" ++ var ++ "'"
  TypeMismatch expected actual ->
    formatError $ "expected " ++ lowercase expected ++ ", but got " ++ lowercase actual
 where
  lowercase x = map C.toLower (show x)

  startPosLine = unPos $ sourceLine start
  startPosColumn = unPos $ sourceColumn start
  endPosLine = unPos $ sourceLine end
  endPosColumn = unPos $ sourceColumn end

  frontSpace = replicate (length $ show endPosLine) ' '
  divider = " | "

  fileLine l = (lines file) !! (l - 1)
  fileLineLength = length . fileLine

  columnOffset l
    | l == startPosLine = startPosColumn - 1
    | otherwise = 0
  highlightWidth l
    | startPosLine == endPosLine = endPosColumn - startPosColumn
    | l == startPosLine = fileLineLength l - startPosColumn + 1
    | l == endPosLine = endPosColumn - 1
    | otherwise = fileLineLength l

  markerPadding l = replicate (columnOffset l) ' '
  markerHighlight l = replicate (highlightWidth l) '^'

  numberedSourceLine l = show l ++ divider ++ fileLine l
  errorMarker l = frontSpace ++ divider ++ markerPadding l ++ markerHighlight l

  formatErrorLine l = numberedSourceLine l ++ "\n" ++ errorMarker l
  errorLines = map formatErrorLine [startPosLine .. endPosLine]

  formatError msg =
    unlines
      [ sourceName start ++ ":" ++ show startPosLine ++ ":" ++ show startPosColumn
      , frontSpace ++ divider
      , L.intercalate "\n" errorLines
      , msg
      ]
