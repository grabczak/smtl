module Main (main) where

import Text.Megaparsec

import Parser
import TypeChecker

main :: IO ()
main = do
  let path = "./example.smtl"
  content <- readFile path
  case parseProgram path content of
    Left e -> putStrLn $ errorBundlePretty e
    Right p -> case checkProgram p of
      Left e -> putStrLn $ "Type error: " ++ show e
      Right typedProgram -> putStrLn $ show typedProgram
