{-# LANGUAGE GHC2024 #-}

module Main (main) where

import Control.Exception
import System.Environment
import System.Exit
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

handleError :: SomeException -> IO a
handleError e = putStrLn (show e) >> exitFailure

main :: IO ()
main = do
  paths <- getArgs
  case paths of
    [] -> putStrLn "Please provide a file path as an argument."
    (path : _) -> do
      content <- catch (readFile path) handleError
      case parseProgram path content of
        Left parseErr -> putStrLn $ errorBundlePretty parseErr
        Right program -> do
          case checkProgram program of
            Left semanticErr -> putStrLn $ semanticErrorPretty content semanticErr
            Right correctProgram -> do
              let z3Input = smtlib correctProgram
              putStrLn "Z3 Input:\n"
              putStrLn z3Input
              putStrLn "Z3 Output:\n"
              z3Output <- catch (callZ3 z3Input) handleError
              putStr z3Output
