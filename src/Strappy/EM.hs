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
import Strappy.Config

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


-- | Performs one iteration of EM on multitask learning
-- Does LP construction rather than hill-climbing
doEMIter :: [(Expr -> Double, Type)] -- ^ Tasks
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
doEMIter tasks lambda pseudocounts frontierSize grammar = do
  -- For each type, sample a frontier
  frontiers <- mapM (\tp -> (if sampleByEnumeration then sampleBFM else sampleExprs) frontierSize grammar tp
                            >>= return . (tp,))
               $ nub $ map snd tasks
  -- If we're sampling, the number of unique expressions is not given;
  -- display them.
  unless sampleByEnumeration $
    putStrLn $ "Frontier sizes: " ++ (unwords $ map (show . Map.size . snd) frontiers)
  -- For each task, compute the P(t|e) terms
  let rewardedFrontiers = Prelude.flip map tasks $ \ (tsk, tp) ->
        Map.mapWithKey (\expr cnt -> (cnt, log (tsk expr))) $ fromJust $ lookup tp frontiers
  -- For each task, weight the corresponding frontier by P(e|g)
  let weightedFrontiers = Prelude.flip map rewardedFrontiers $ Map.map (\(cnt, logLikelihood) -> cnt + logLikelihood)
  -- Normalize frontiers
  let logZs = map (Map.fold logSumExp (log 0.0)) weightedFrontiers
  let weightedFrontiers' = zipWith (\logZ -> filter (\(_,x) -> not (isNaN x) && not (isInfinite x)) . Map.toList .
                                             Map.map (\x-> x-logZ))
                                   logZs weightedFrontiers
  let numHit = length $ filter id $ Prelude.flip map rewardedFrontiers $
        Map.fold (\(_, ll) acc -> acc || ll >= -0.0001) False
  putStrLn $ "Completely solved " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."
  let obs = foldl (\acc frontier ->
                    Map.unionWith logSumExp acc $ Map.fromList frontier) Map.empty weightedFrontiers'
  -- Exponentiate log likelihoods to get final weights
  let obs' = map (\(e,logW) -> (e, exp logW)) $
             filter (\(_,w) -> not (isNaN w) && not (isInfinite w)) $
             Map.toList obs
  if length obs' == 0
    then do putStrLn "Hit no tasks."
            return grammar -- Didn't hit any tasks
    else do let grammar' = compressWeightedCorpus lambda pseudocounts grammar obs'
            let terminalLen = length $ filter isTerm $ Map.keys $ grExprDistr grammar
            putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - terminalLen - 1) ++ " new productions."
            putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar')
            when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'
            putStrLn "" -- newline
            return grammar'


-- Polynomial regression test for EM
polyEM :: IO ()
polyEM = do
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- polyExprs ] }
  -- Make nth order polynomial task with fixed coefficients
  let mkNthDet :: (Int -> Int) -> (Expr -> Double, Type)
      mkNthDet poly = let loss proc = sum $ zipWith (\a b -> (a-b)*(a-b)) (map (fromIntegral . eval proc) [0..9]) (map (fromIntegral . poly) [0..9])
                          score proc = exp $ - loss proc
                      in (score, tInt ->- tInt)
  let const = [ mkNthDet (\_ -> x) | x <- [0..9] ]
  let lin = [ mkNthDet (\a -> x * a + y) | x <- [1..9], y <- [0..9] ]
  let quad = [ mkNthDet (\a -> x * a * a + y * a + z) | x <- [1..9], y <- [0..9], z <- [0..9] ]
  loopM seed [0..14] $ \grammar step -> do
    putStrLn $ "EM Iteration: " ++ show step
    grammar' <- doEMIter (const++lin++quad) 1.5 1.0 frontierSize grammar
    return grammar'
  return ()
                    
main = polyEM
