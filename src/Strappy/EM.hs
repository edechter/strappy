{-# LANGUAGE TupleSections  #-}

--module Strappy.EM where
module Main where

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
doEMIter :: Type -- ^ Type of the tasks
            -> (Expr -> a) -- ^ Procedure for compiling expressions
            -> [a -> Double] -- ^ Tasks (functions from compiled expressions to log likelihoods)
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
doEMIter reqTp compile tasks lambda pseudocounts frontierSize grammar = do
  frontier <- (if sampleByEnumeration then sampleBFM else sampleExprs) frontierSize grammar reqTp
  -- If we're sampling, the number of unique expressions is not given;
  -- display them.
  unless sampleByEnumeration $
    putStrLn $ "Frontier size: " ++ (show $ Map.size frontier)
  putStrLn $ "Frontier entropy: " ++ (show $ entropyLogDist $ Map.elems frontier)
  -- Compile all of the sampled programs
  let frontier' = map (\(e, w) -> (e, w, compile e)) $ Map.toList frontier
  -- For each task, compute the P(t|e) terms
  let taskFrontiers = map (\tsk -> map (\(e, w, cE) -> (e, w, cE, tsk cE)) frontier') tasks
  -- Compute the distribution over programs for each task
  let taskDistrs = map (\taskFront -> normalizeDist $ map (\(e, w, _, ll) -> (e, w+ll)) taskFront)
                       taskFrontiers
  -- Display the number of tasks hit
  let numHit = length $ filter (any (\(_, _, _, ll) ->
                                      not (isNaN ll) && not (isInfinite ll) && ll > -0.001)) taskFrontiers
  putStrLn $ "Completely solved " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."  
  -- Exponentiate log likelihoods to get final weights
  let obs = foldl (\acc frontier ->
                    Map.unionWith logSumExp acc $ Map.fromList frontier) Map.empty taskDistrs
  let obs' = map (\(e,logW) -> (e, exp logW)) $
             filter (\(_,w) -> not (isNaN w) && not (isInfinite w)) $
             Map.toList obs
  -- Finally, we build the new grammar from the observations (obs')
  let grammar' = compressWeightedCorpus reqTp lambda pseudocounts grammar obs'
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
  let compilePoly :: Expr -> Maybe [Int]
      compilePoly e = let points = map (\x -> timeLimitedEval $ e <> cInt2Expr x) [0..9] in
                      if all isJust points then Just (map fromJust points) else Nothing
  -- Make nth order polynomial task with fixed coefficients
  let mkNthDet :: (Int -> Int) -> (Maybe [Int] -> Double)
      mkNthDet poly = let points = map poly [0..9] in
                      maybe (log 0) (fromIntegral . negate . sum . map (\x->x*x) . zipWith (-) points)
  let dets = [ mkNthDet (\a -> x * a * a + y * a + z) | x <- [0..9], y <- [0..9], z <- [0..9] ]
  loopM seed [0..20] $ \grammar step -> do
    putStrLn $ "EM Iteration: " ++ show step
    grammar' <- doEMIter (tInt ->- tInt) compilePoly dets 2.0 1.0 frontierSize grammar
    saveGrammar ("poly_grammar_" ++ show step) grammar'
    return grammar'
  return ()

main = polyEM
