module Main where

import Strappy.Expr
import Strappy.Type
import Strappy.Utils
import Strappy.Library
import Strappy.BeamSearch
import Strappy.Planner
import Strappy.EM
import Strappy.ProdFeat
import Strappy.Sample

import qualified Data.Map as Map
import System.Environment
import System.Random
import Data.Maybe
import Control.Monad
import Debug.Trace
import Data.List
import Data.Array

import Strappy.EnumBits

makePolyTask :: Int -> Int -> Int -> EMTask
makePolyTask a b c = EMTask { etName = "p_" ++ show a ++ "_" ++ show b ++ "_" ++ show c,
                             etLogLikelihood =
                              let correct = map Just [ a * x * x + b * x + c | x <- [1..4] ] :: [Maybe Int]
                              in \e -> let res = [ timeLimitedEval (e <> cInt2Expr i) | i <- [1..4] ]
                                       in if correct == res
                                          then 0.0
                                          else log 0.0,
                             etType = tInt ->- tInt }


main = do
  args@[rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
  putStrLn $ "Poly (EC) run with: " ++ unwords args
  setStdGen $ mkStdGen $ read rndSeed

  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- polyExprs ] }
  let tasks = [ makePolyTask a b c | a <- [0..3], b <- [0..3], c <- [0..3] ]
  good <- loopM seed [0..4] $ \grammar step -> do
    putStrLn $ "EM Iteration: " ++ show step
    grammar' <- doEMIter (prefix++"/best_"++show step) tasks
                         (read lambda) (read pseudocounts) (read fSize) grammar
    saveGrammar (prefix++"/grammar_" ++ show step) grammar'
    return grammar'
  let ps = enumBits good 7500 (tInt ->- tInt)
  forM_ tasks $ \tsk -> do
    let ps' = filter (\p -> not (isInvalidNum $ etLogLikelihood tsk p)) ps
    if null ps'
      then putStrLn $ (etName tsk) ++ " = []"
      else putStrLn $ (etName tsk) ++ " = " ++ show (taskFeatures good (tInt ->- tInt) ps')
  return ()