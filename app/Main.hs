module Main (main) where

import System.IO
import System.Process
import Text.Megaparsec

import Display
import Parser
import Solver
import TypeChecker

callZ3 :: String -> IO String
callZ3 input = do
  (Just hin, Just hout, _, _) <-
    createProcess
      (proc "z3" ["-in"])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        }
  hPutStr hin input
  hClose hin
  hGetContents hout

main :: IO ()
main = do
  let path = "./example.smtl"
  content <- readFile path
  case parseProgram path content of
    Left parseErr -> putStrLn $ errorBundlePretty parseErr
    Right program -> do
      putStrLn $ show program
      case checkProgram program of
        Left typeErr -> putStrLn $ typeErrorPretty content typeErr
        Right correctProgram -> do
          let z3Input = smtlib correctProgram
          putStrLn $ z3Input
          z3Result <- callZ3 z3Input
          putStrLn z3Result
