module Main (main) where

import Data.Void
import Text.Megaparsec

import Parser

smtl :: String -> String -> Either (ParseErrorBundle String Void) Program
smtl path content = parse program path content

main :: IO ()
main = do
  let path = "./example.smtl"
  content <- readFile path
  case smtl path content of
    Left e -> putStrLn $ errorBundlePretty e
    Right p -> print $ p
