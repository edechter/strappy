{-# LANGUAGE TupleSections  #-}
module Main where
--module Strappy.EM where

import Strappy.Sample
import Strappy.Expr
import Strappy.Library
import Strappy.Type
import Strappy.Task
import Strappy.Utils
import Strappy.Library 
import Strappy.LPCompress

import Data.Maybe
import Data.List
import Data.Function
import Control.Arrow (first)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Debug.Trace 

import System.CPUTime


-- | Performs one iterations of EM on multitask learning
-- Does LP construction rather than hill-climbing
doEMIter :: [(Expr -> Double, Type)] -- ^ Tasks
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
doEMIter tasks lambda pseudocounts frontierSize grammar = do
  -- For each type, sample a frontier
  let frontiers = map (\tp -> (tp, sampleBF frontierSize grammar tp))
                  $ nub $ map snd tasks
  -- For each task, weight the corresponding frontier by likelihood
  let weightedFrontiers = Prelude.flip map tasks $ \(tsk, tp) ->
        Map.mapWithKey (\expr cnt -> cnt + log (tsk expr)) $ fromJust $ lookup tp frontiers
  -- Normalize frontiers
  let logZs = map (Map.fold logSumExp (log 0.0)) weightedFrontiers
  let weightedFrontiers' = zipWith (\logZ -> filter (\(_,x) -> not (isNaN x) && not (isInfinite x)) . Map.toList .
                                             Map.map (\x-> x-logZ))
                                   logZs weightedFrontiers
  let numHit = length $ filter (not . null) weightedFrontiers'
  putStrLn $ "Hit " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."
  let obs = foldl (\acc frontier ->
                    Map.unionWith logSumExp acc $ Map.fromList frontier) Map.empty weightedFrontiers'
  -- Exponentiate log likelihoods to get final weights
  let obs' = map (\(e,logW) -> (e, exp logW)) $
             filter (\(_,w) -> not (isNaN w) && not (isInfinite w)) $
             Map.toList obs
  if length obs' == 0
    then do putStrLn "Hit no tasks."
            return grammar -- Didn't hit any tasks
    else do let subtrees = foldl1 (Map.unionWith (+)) $ map (countSubtrees Map.empty) obs'
            let terminals = filter isTerm $ Map.keys $ grExprDistr grammar
            let newProductions = compressLP lambda subtrees
            let productions = newProductions ++ terminals
            let uniformLogProb = -log (genericLength productions)
            let grammar'  = Grammar (log 0.5) $ Map.fromList [ (prod, uniformLogProb) | prod <- productions ]
            let grammar'' = inoutEstimateGrammar grammar' pseudocounts obs'
            putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - length terminals - 1) ++ " new productions."
            putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar'')
            putStrLn $ showGrammar $ removeSubProductions grammar''
            return grammar''
         
-- Library for testing EM+polynomial regression
-- This is what was used in the IJCAI paper
polyExprs :: [Expr]
polyExprs = [cI, 
              cS, 
              cB, 
              cC, 
--              cK, 
              cPlus,
              cTimes
              ] ++ [ cInt2Expr 0, cInt2Expr 1 ]


-- Polynomial regression test for EM
polyEM :: IO ()
polyEM = do
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- polyExprs ] }
  -- Make nth order polynomial task with fixed coefficients
  let mkNthDet :: [Int] -> (Expr -> Double, Type)
      mkNthDet coeffs = let coeffs' = reverse coeffs
                            poly :: Int -> Int
                            poly x = sum $ zipWith (*) coeffs' $ map (x^) [0..]
                            score proc = if map (eval proc) [0..9] == map poly [0..9]
                                         then 1.0
                                         else 0.0
                        in (score, tInt ->- tInt)
  let const = [ mkNthDet [x] | x <- [1..9] ]
  let lin = [ mkNthDet [x,y] | x <- [1..9], y <- [0..9] ]
  let quad = [ mkNthDet [x,y,z] | x <- [1..9], y <- [0..9], z <- [0..9] ]
  loopM seed [0..20] $ \grammar step -> do
    putStrLn $ "EM Iteration: " ++ show step
    grammar' <- doEMIter (const++lin++quad) 2.0 1.0 5000 grammar
    return grammar'
  return ()
                    
main = polyEM
