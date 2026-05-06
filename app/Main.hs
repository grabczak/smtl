module Main (main) where

import System.IO
import System.Process
import Text.Megaparsec

import Parser
import Solver
import TypeChecker

callZ3 :: String -> IO String
callZ3 input = do
  (Just stdin, Just stdout, _, _) <-
    createProcess
      (proc "z3" ["-in"])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        }
  hPutStr stdin input
  hPutStr stdin "(exit)\n" -- z3 needs exit command
  hClose stdin
  hGetContents stdout

main :: IO ()
main = do
  let path = "./example.smtl"
  content <- readFile path
  case parseProgram path content of
    Left e -> putStrLn $ errorBundlePretty e
    Right p -> case checkProgram p of
      Left e -> putStrLn $ typeErrorPretty content e
      Right p' -> do
        let z3Input = smtlib p'
        result <- callZ3 z3Input
        putStrLn result
