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

{--- | Greedily chooses those productions that, with one step of lookahead, lead to compression
greedyGrammar :: Double -> -- ^ lambda
                 Double -> -- ^ pseudocounts
                 [(Expr, Double)] -> -- ^ weighted observations
                 Grammar -- ^ Grammar
greedyGrammar lambda pseudocounts grammar obs =
  let subtreeCounts = Map.unionWith (+) $ map (Map.empty) obs -- Find common subtrees, and how often they occur
-}


-- | Performs one iterations of EM on multitask learning
-- Does greedy grammar construction rather than hill-climbing
doEMIter :: [(Expr -> Double, Type)] -- ^ Tasks
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
doEMIter tasks lambda pseudocounts frontierSize grammar = do
  -- For each type, sample a frontier
  frontiers <- mapM (\tp -> do sample <- return $ sampleBF frontierSize grammar tp
                               return (tp, sample))
                    $ nub $ map snd tasks
  -- For each task, weight the corresponding frontier by likelihood
  let weightedFrontiers = Prelude.flip map tasks $ \(tsk, tp) ->
        Map.mapWithKey (\expr cnt -> cnt + log (tsk expr)) $ fromJust $ lookup tp frontiers
  -- Normalize frontiers
  let logZs = map (Map.fold logSumExp (log 0.0)) weightedFrontiers
  let weightedFrontiers' = zipWith (\logZ -> filter ((>0) . snd) . Map.toList .
                                             Map.map (\x-> if isNaN x || isInfinite x then 0.0 else exp (x-logZ)))
                                   logZs weightedFrontiers
  let numHit = length $ filter (not . null) weightedFrontiers'
  putStrLn $ "Hit " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."
  let obs = foldl (\acc (logZ, frontier) ->
                    if not (isNaN logZ) && not (isInfinite logZ) && not (null frontier)
                    then Map.unionWith logSumExp acc $ Map.map (\x -> x-logZ) $ Map.fromList frontier
                    else acc) Map.empty $ zip logZs weightedFrontiers'
  -- Exponentiate log likelihoods to get final weights
  let obs' = map (\(e,logW) -> (e, exp logW)) $
             filter (\(_,w) -> not (isNaN w) && not (isInfinite w)) $
             Map.toList obs
  putStrLn $ "Total mass: " ++ show (sum $ map snd obs')
  if length obs' == 0
    then do putStrLn "Hit no tasks."
            return grammar -- Didn't hit any tasks
    else do let subtrees = foldl1 (Map.unionWith (+)) $ map (countSubtrees Map.empty) obs'
            let terminals = filter isTerm $ Map.keys $ grExprDistr grammar
            let productions = compressLP lambda subtrees ++ terminals
            let uniformLogProb = -log (genericLength productions)
            let grammar'  = Grammar (log 0.45) $ Map.fromList [ (prod, uniformLogProb) | prod <- productions ]
            let grammar'' = inoutEstimateGrammar grammar' pseudocounts obs'
            putStrLn $ "Got " ++ show (length $ lines $ showGrammar $ removeSubProductions grammar') ++ " new productions."
            putStrLn $ showGrammar $ removeSubProductions grammar'
            return grammar'
         
-- Library for testing EM+polynomial regressionx
polyExprs :: [Expr]
polyExprs = [cI, 
              cS, 
              cB, 
              cC, 
              cK, 
              cPlus,
              cTimes
              ] ++ cInts


-- Polynomial regression test for EM
polyEM :: IO ()
polyEM = do
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- polyExprs ] }
  -- Make nth order polynomial task with random coefficients
{-  let mkNthOrder :: Int -> IO (Expr -> Double, Type)
      mkNthOrder n = replicateM (n+1) $ randomRIO (0,9::Int) >>= \coeffs ->
        let poly :: Int -> Int
            poly x = sum $ zipWith (*) coeffs $ map (x^) [0..]
            score proc =
              if map (eval proc) [0..3] == map poly [0..3]
              then 1.0
              else 0.0
        in return (score, tInt ->- tInt)
-}
  -- Make nth order polynomial task with fixed coefficients
  let mkNthDet :: [Int] -> (Expr -> Double, Type)
      mkNthDet coeffs = let poly :: Int -> Int
                            poly x = sum $ zipWith (*) coeffs $ map (x^) [0..]
                            score proc = if map (eval proc) [0..3] == map poly [0..3]
                                         then 1.0
                                         else 0.0
                        in (score, tInt ->- tInt)
  let const = [ mkNthDet [x] | x <- [1..9] ]
  let lin = [ mkNthDet [x,y] | x <- [1..9], y <- [1..9] ]
  let quad = [ mkNthDet [x,y,z] | x <- [1..9], y <- [1..9], z <- [1..3] ]
  loopM seed [1..20] $ \grammar step -> do
    putStrLn $ "EM Iteration: " ++ show step
    grammar' <- doEMIter (const++lin) 6000 1.0 5000 grammar
    return grammar'
  return ()
                    
main = polyEM
