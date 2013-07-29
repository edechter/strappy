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
import System.Environment


-- | Performs one iteration of EM on multitask learning
doEMIter :: String -> -- ^ Prefix for log output
            Type -- ^ Type of the tasks
            -> (Expr -> a) -- ^ Procedure for compiling expressions
            -> [a -> Double] -- ^ Tasks (functions from compiled expressions to log likelihoods)
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
doEMIter prefix reqTp compile tasks lambda pseudocounts frontierSize grammar = do
    -- Sample a frontier
  frontier <- (if sampleByEnumeration then sampleBitsM else sampleExprs) frontierSize grammar reqTp
  -- If we're sampling, the number of unique expressions is not given;
  -- display them.
  unless sampleByEnumeration $
    putStrLn $ "Frontier size: " ++ show (Map.size frontier)
  putStrLn $ "Frontier entropy: " ++ show (entropyLogDist (Map.elems frontier))
  -- Compile each program
  let compFrontier = Map.mapWithKey (\e w -> (compile e, w)) frontier
  -- For each task, compute the P(t|e) terms
  let rewardedFrontiers = flip map tasks $ \ tsk ->
        Map.mapWithKey (\expr (cExpr, w) -> (w, log $ tsk cExpr)) compFrontier
  -- For each task, weight the corresponding frontier by P(e|g)
  let weightedFrontiers = flip map rewardedFrontiers $ Map.map (uncurry (+))
  -- Normalize frontiers
  let logZs = map (Map.fold logSumExp (log 0.0)) weightedFrontiers
  let weightedFrontiers' = zipWith (\logZ -> filter (\(_,x) -> not (isNaN x) && not (isInfinite x)) . Map.toList .
                                             Map.map (\x-> x-logZ))
                                   logZs weightedFrontiers
  let numHit = length $ filter id $ flip map rewardedFrontiers $
                any ((>= -0.001) . snd . snd) . Map.toList
  putStrLn $ "Completely solved " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."
  -- Save out the best program for each task to a file
  saveBest prefix rewardedFrontiers
  let obs = foldl (\acc frontier ->
                    Map.unionWith logSumExp acc $ Map.fromList frontier) Map.empty weightedFrontiers'
  -- Exponentiate log likelihoods to get final weights
  let obs' = map (\(e,logW) -> (e, exp logW)) $
             filter (\(_,w) -> not (isNaN w) && not (isInfinite w)) $
             Map.toList obs
  if length obs' == 0
    then do putStrLn "Hit no tasks."
            return grammar -- Didn't hit any tasks
    else do let grammar' = compressWeightedCorpus undefined lambda pseudocounts grammar obs'
            let terminalLen = length $ filter isTerm $ Map.keys $ grExprDistr grammar
            putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - terminalLen - 1) ++ " new productions."
            putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar')
            when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'
            putStrLn "" -- newline
            return grammar'
            
saveBest fname fronts =
  let fronts' = map Map.toList fronts
      bestprogs = map (\front -> if null front
                                  then Nothing
                                  else Just $ maximumBy (\(_, (w,ll)) (_, (w',ll')) -> compare (w+ll) (w'+ll')) front)
                                  fronts'
      str = map (maybe "Miss" (\(e, (w,ll)) -> show e ++ "\t" ++ show w ++ "\t" ++ show ll)) bestprogs
  in writeFile fname (unlines str)

-- Polynomial regression test for EM
polyEM :: IO ()
polyEM = do
  [lambda, pseudocounts, fSize, prefix] <- getArgs
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- polyExprs ] }
  let compilePoly :: Expr -> Maybe [Int]
      compilePoly e = let points = map (\x -> timeLimitedEval $ e <> cInt2Expr x) [0..9] in
                      if all isJust points then Just (map fromJust points) else Nothing
  -- Make nth order polynomial task with fixed coefficients
  let mkNthDet :: (Int -> Int) -> (Maybe [Int] -> Double)
      mkNthDet poly = let points = map poly [0..9] in
                      maybe 0 (exp . fromIntegral . negate . sum . map (\x->x*x) . zipWith (-) points)
  let dets = [ mkNthDet (\a -> x * a * a + y * a + z) | x <- [0..9], y <- [0..9], z <- [0..9] ]
  loopM seed [0..20] $ \grammar step -> do
    putStrLn $ "EM Iteration: " ++ show step
    grammar' <- doEMIter (prefix++"_"++show step) (tInt ->- tInt) compilePoly dets
                         (read lambda) (read pseudocounts) (read fSize) grammar
    saveGrammar (prefix++"poly_grammar_" ++ show step) grammar'
    return grammar'
  return ()

main = polyEM
