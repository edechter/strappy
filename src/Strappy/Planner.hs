{-# LANGUAGE TupleSections  #-}

module Planner where

import Strappy.Sample
import Strappy.Expr
import Strappy.Library
import Strappy.Type
import Strappy.Task
import Strappy.Utils
import Strappy.Library 
import Strappy.LPCompress
import Strappy.Config


import Control.Monad
import Control.Monad.Random
import qualified Data.Map as Map
import Data.List
import Data.Maybe


mcmcPlan :: MonadRandom m =>
            Expr -> -- ^ Initial plan, such as the empty list, or the identity function
            [(Expr, Double)] -> -- ^ Distribution over expressions drawn from the grammar
            (Expr -> Double) -> -- ^ Log Likelihood function
            Int -> -- ^ length of the plan
            m (Double, [(Expr, Double)]) -- ^ log partition function, expressions and log rewards
mcmcPlan e0 dist likelihood len =
  mcmc e0 1 0
  where mcmc prefix prefixRecipLike lenSoFar =
          -- Reweight by the likelihood
          let reweighted = map (\(e, w) ->
                                 let like = likelihood $ e <> prefix
                                 in ((e, like), like + w)) dist
          in do (e, eLike) <- sampleMultinomialLogProb reweighted
                -- (possibly) recurse
                (suffixLogPartition, suffixRewards) <-
                  if lenSoFar < len - 1
                  then mcmc (e <> prefix) (prefixRecipLike - eLike) (len+1)
                  else return (0, [])
                let partitionFunction = logSumExp suffixLogPartition prefixRecipLike
                return (partitionFunction, (e, partitionFunction):suffixRewards)


doEMPlan :: [(Expr -> Double, Expr, Type)] -- ^ Tasks
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Int -- ^ number of plans sampled per task
            -> Int -- ^ max length of each plan
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
doEMPlan tasks lambda pseudocounts frontierSize numPlans planLen grammar = do
  -- For each type, sample a frontier of actions
  frontiers <- mapM (\tp -> (if sampleByEnumeration then sampleBFM else sampleExprs)
                            frontierSize grammar (tp ->- tp)
                            >>= return . (tp,) . Map.toList)
               $ nub $ map (\(_,_,tp)->tp) tasks
  -- For each task, do greedy stochastic search to get candidate plans
  -- Each task records all of the programs used in successful plans, as well as the associated rewards
  -- These are normalized to get a distribution over plans, which gives weights for the MDL's of the programs
  normalizedRewards <- forM tasks $ \(likelihood, seed, tp) -> do
    let frontier = snd $ fromJust $ find ((==tp) . fst) frontiers
    (logPartitionFunction, programLogRewards) <-
      foldM (\ (logZ, rewards) _ -> mcmcPlan seed frontier likelihood planLen >>=
                                 \(logZ', rewards') -> return (logSumExp logZ logZ', rewards++rewards'))
            (log 0.0, []) [1..numPlans]
    -- normalize rewards
    return $ map (\(e, r) -> (e, exp (r - logPartitionFunction))) programLogRewards
  -- Compress the corpus
  let normalizedRewards' = Map.toList $ Map.fromListWith (+) $ concat normalizedRewards
  let grammar' = compressWeightedCorpus lambda pseudocounts grammar normalizedRewards'
  let terminalLen = length $ filter isTerm $ Map.keys $ grExprDistr grammar
  putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - terminalLen - 1) ++ " new productions."
  putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar')
  when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'
  putStrLn "" -- newline
  return grammar'
