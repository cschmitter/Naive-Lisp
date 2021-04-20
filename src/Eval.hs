-- |
module Eval where

import Control.Monad.ST
import Data.STRef
import GHC.Float (int2Double)
import Parse

testEval :: [Expr] -> IO ()
testEval as = do
  line <- getLine
  case line of
    ":q" -> return ()
    ":e" -> do
      print $ runEval as
      testEval []
    _ ->
      case myParse pExpr line of
        Right a -> do
          testEval $ as ++ [a]
        Left b -> print b

runEval :: [Expr] -> Literal
runEval es =
  runST $ do
    fs <- newSTRef stdFLib
    ns <- newSTRef stdNLib
    runEval' fs ns es

runEval' ::
     STRef s [(String, [String], Expr)]
  -> STRef s [(String, Literal)]
  -> [Expr]
  -> ST s Literal
--
runEval' _ ns [Name name] = do
  cns <- readSTRef ns
  return . snd . head $ filter ((== name) . fst) cns
--
runEval' fs ns [Func f_name params] = do
  lParams <- traverse (\x -> runEval' fs ns [x]) params
  case handleUtility fs ns f_name lParams of
    Just a -> a
    Nothing -> do
      cfs <- readSTRef fs
      cns <- readSTRef ns
      -- utility function
      let fst3 (a, _, _) = a
      -- get args and e for called function <f_name>
      let (_, args, e) = head $ filter ((== f_name) . fst3) cfs
      let aeParams = zip args (map L lParams)
      let exprs = map (uncurry Let) aeParams
      result <- runEval' fs ns (exprs ++ [e])
      modifySTRef' fs (const cfs)
      modifySTRef' ns (const cns)
      return result --
runEval' fs ns ((Let name expr):es) = do
  l <- runEval' fs ns [expr]
  modifySTRef' ns ((name, l) :)
  runEval' fs ns es
  --
runEval' fs ns ((Def f_name args result):es) = do
  modifySTRef' fs ((f_name, args, result) :)
  runEval' fs ns es
--
runEval' fs ns ((Do nes):es) = runEval' fs ns (nes ++ es)
--
runEval' fs ns ((If condition true false):es) = do
  c <- runEval' fs ns [condition]
  case c of
    Int 1 -> runEval' fs ns (true:es)
    _ -> runEval' fs ns (false:es)
--
runEval' fs ns [e] = eval e
  where
    eval (Paren ne) = runEval' fs ns [ne]
    eval (L l) = return l
    eval _ = undefined --
runEval' fs ns (_:es) = runEval' fs ns es
--
runEval' _ _ [] = return . Int $ negate 1

stdFLib :: [(String, [String], Expr)]
stdFLib = [("fst", ["a", "b"], Name "a"), ("snd", ["a", "b"], Name "b")]

stdNLib :: [(String, Literal)]
stdNLib = [("pi", Double 3.14)]

handleUtility ::
     STRef s [(String, [String], Expr)]
  -> STRef s [(String, Literal)]
  -> String
  -> [Literal]
  -> Maybe (ST s Literal)
handleUtility _ _ "add" [Int a, Int b] = Just . return $ Int (a + b)
handleUtility _ _ "add" [Double a, Int b] =
  Just . return $ Double (a + int2Double b)
handleUtility _ _ "add" [Int a, Double b] =
  Just . return $ Double (int2Double a + b)
handleUtility _ _ "add" [Double a, Double b] = Just . return $ Double (a + b)
handleUtility _ _ "eq" (a:as) =
  Just . return $
  if all (== a) as
    then Int 1
    else Int 0
handleUtility _ _ "if" [Int a, b, c] =
  Just . return $
  if a == 0
    then c
    else b
handleUtility _ _ "push" [a, List as] = Just . return $ List (a:as)
handleUtility _ _ "head" [List (a:_)] = Just . return $ a
handleUtility _ _ "tail" [List (_:as)] = Just . return $ List as
handleUtility _ _ "append" [List as, a] = Just . return $ List (as++[a])
handleUtility _ _ "get" [Int n, List as] = Just . return $ as !! n
handleUtility _ _ _ _ = Nothing
