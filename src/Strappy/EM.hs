{-# LANGUAGE TupleSections  #-}

module Strappy.EM where

import Strappy.Sample
import Strappy.Expr
import Strappy.Library
import Strappy.Type
import Strappy.Task
import Strappy.Utils
import Strappy.Library 
import Strappy.Compress
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

data EMTask = EMTask { etName :: String,
                       etLogLikelihood :: Expr -> Double,
                       etType :: Type }

-- | Performs one iteration of EM on multitask learning
doEMIter :: String -- ^ Prefix for log output
            -> [EMTask] -- ^ Tasks (functions from compiled expressions to log likelihoods)
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
doEMIter prefix tasks lambda pseudocounts frontierSize grammar = do
    -- Sample frontiers
  let sampler = if sampleByEnumeration then sampleBitsM else sampleExprs
  frontiers <- mapM (\tp -> sampler frontierSize grammar tp >>= return . (tp,)) $ nub (map etType tasks)
  let lookupFrontier tp = fromJust $ lookup tp frontiers
  -- If we're sampling, the number of unique expressions is not given;
  -- display them.
  unless sampleByEnumeration $
    putStrLn $ "Frontier sizes: " ++ show (map (Map.size . snd) frontiers)
  putStrLn $ "Frontier entropies: " ++ show (map (entropyLogDist . Map.elems . snd) frontiers)
  -- For each task, compute the P(t|e) terms
  let rewardedFrontiers = flip map tasks $ \ tsk ->
        (etName tsk,
          Map.mapWithKey (\expr w -> (w, etLogLikelihood tsk expr)) (lookupFrontier $ etType tsk))
  let frontierLikelihoods = flip map rewardedFrontiers $ \ (_, mp) -> Map.map snd mp
  let frontierLikelihoods' = flip filter frontierLikelihoods $ \mp -> any (not . isInvalidNum) (Map.elems mp)
  -- For each task, weight the corresponding frontier by P(e|g)
  let weightedFrontiers = flip map rewardedFrontiers $ Map.map (uncurry (+)) . snd
  -- Normalize frontiers
  let logZs = map (Map.fold logSumExp (log 0.0)) weightedFrontiers
  let weightedFrontiers' = zipWith (\logZ -> filter (\(_,x) -> not (isNaN x) && not (isInfinite x)) . Map.toList .
                                             Map.map (\x-> x-logZ))
                                   logZs weightedFrontiers
  -- numHit = convert the rewardedFrontiers to an association list, each value will be a pair, take the second member of each pair, look to see whether it is a valid value, see if it is high enough be an answer, save those that are, and consider them hit.
  let numHit = length $ filter id $ flip map rewardedFrontiers $ \ (_, mp) -> any (\x-> x >= -0.01) $ filter (\x -> not (isNaN x) && not (isInfinite x)) $ map (snd . snd) $ Map.toList mp
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
    else do let grammar' = grammarEM lambda pseudocounts (blankLibrary grammar) frontierLikelihoods' --compressWeightedCorpus lambda pseudocounts grammar obs'
            let terminalLen = length $ filter isTerm $ Map.keys $ grExprDistr grammar
            putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - terminalLen - 1) ++ " new productions."
            putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar')
            when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'
            putStrLn "" -- newline
            return grammar'
            
saveBest fname fronts =
  let fronts' = map (\(nm, es) -> let es' = filter (\(_, (w, ll)) -> not (isNaN w) && not (isNaN ll) &&
                                                                     not (isInfinite w) && not (isInfinite ll))
                                                   $ Map.toList es
                                  in (nm, es')) fronts
      bestprogs = map (\(nm, front) -> if null front
                                       then (nm, Nothing)
                                       else (nm, Just $ maximumBy (\(_, (w,ll)) (_, (w',ll')) -> compare (ll) (ll')) front))
                      fronts'
      str = flip map bestprogs $ \(nm, result) ->
                                 maybe ("Missed "++nm) (\(e, (w,ll)) -> nm ++ "\t" ++ show e ++ "\t" ++ show ll) result
  in writeFile fname (unlines str)
