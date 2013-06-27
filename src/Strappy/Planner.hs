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


-- | TODO: I don't use log probabilities anywhere here; probably should, in order to avoid floating-point issues.
-- |       There aren't any status updates printed out.
-- |       The code for compressing a corpus is copied from EM.hs; it should be turned in to its own procedure.

mcmcPlan :: MonadRandom m =>
            Expr -> -- ^ Initial plan, such as the empty list, or the identity function
            [(Expr, Double)] -> -- ^ Distribution over expressions drawn from the grammar
            (Expr -> Double) -> -- ^ Likelihood function
            Int -> -- ^ length of the plan
            m (Double, [(Expr, Double)]) -- ^ partition function, expressions and rewards
mcmcPlan e0 dist likelihood len =
  mcmc e0 1 0
  where mcmc prefix prefixRecipLike lenSoFar =
          -- Reweight by the likelihood
          let reweighted = map (\(e, w) ->
                                 let like = likelihood $ e <> prefix
                                 in ((e, like), like*w)) dist
          in do (e, eLike) <- sampleMultinomial $ normalizeDist reweighted
                -- (possibly) recurse
                (suffixPartition, suffixRewards) <-
                  if lenSoFar < len - 1
                  then mcmc (e <> prefix) (prefixRecipLike/eLike) (len+1)
                  else return (0, [])
                let partitionFunction = suffixPartition + prefixRecipLike
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
    (partitionFunction, programRewards) <-
      foldM (\ (z, rewards) _ -> mcmcPlan seed frontier likelihood planLen >>=
                                 \(z', rewards') -> return (z+z', rewards++rewards'))
            (0.0, []) [1..numPlans]
    -- normalize rewards
    return $ map (\(e, r) -> (e, r/partitionFunction)) programRewards
  -- Compress the corpus
  let normalizedRewards' = Map.toList $ Map.fromListWith (+) $ concat normalizedRewards
  let subtrees = foldl1 (Map.unionWith (+)) $ map (countSubtrees Map.empty) normalizedRewards'
  let terminals = filter isTerm $ Map.keys $ grExprDistr grammar
  let newProductions = compressLP_corpus lambda subtrees
  let productions = newProductions ++ terminals
  let uniformLogProb = -log (genericLength productions)
  let grammar'   = Grammar (log 0.5) $ Map.fromList [ (prod, uniformLogProb) | prod <- productions ]
  let grammar''  = if pruneGrammar
                   then removeUnusedProductions grammar' $ map fst normalizedRewards'
                   else grammar'
  let grammar''' = inoutEstimateGrammar grammar'' pseudocounts normalizedRewards'
  putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - length terminals - 1) ++ " new productions."
  putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar''')
  when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'''
  putStrLn "" -- newline
  return grammar'''
