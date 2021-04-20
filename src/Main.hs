module Main where

import Parse
import Eval

import System.Environment (getArgs)

main :: IO ()
main = do
  as <- getArgs
  case as of
    "-i":file:_ -> do
      content <- readFile file
      runInterpreter content
    file:_ -> do
      content <- readFile file
      runInterpreter content
    _ -> testEval []

runInterpreter :: String -> IO ()
runInterpreter s = do
  case myParse pExpr ("do\n"++s) of
    Right a -> print $ runEval [a]
    Left b -> print b
