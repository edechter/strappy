module Main where

import Strappy.BottomUp
import Strappy.Expr
import Strappy.Type
import Strappy.Utils
import Strappy.Library

import qualified Data.Map as Map
import System.Environment
import System.Random

makeWordTask :: String -> (Type, Expr, String)
makeWordTask str =
    (tList tChar, e str, str)
    where e [] = cEmpty
          e (c:cs) = (cCons <> cChar2Expr c) <> e cs

main = do
  args@[rndSeed, lambda, pseudocounts, fSize, keepSize, prefix] <- getArgs
  putStrLn $ "Words run with: " ++ unwords args
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- wordExprs ] }
  let tasks = [makeWordTask ("anti"++[suffix]) | suffix <- "abcdefgh" ]
  loopM seed [0..14] $ \grammar step -> do
    putStrLn ("EM Iteration: " ++ show step)
    grammar' <- doBUIter (prefix++"/best_"++show step) tasks
                         (read lambda) (read pseudocounts) (read fSize) (read keepSize) grammar
    saveGrammar (prefix++"/grammar_" ++ show step) grammar'
    return grammar'
  return ()