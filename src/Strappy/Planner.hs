{-# LANGUAGE TupleSections  #-}

module Strappy.Planner where

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
import qualified Control.Monad.Parallel as Parallel
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.IORef


data PlanTask = PlanTask { ptName :: String,
                           ptLogLikelihood :: Expr -> IO Double,
                           ptType :: Type, -- ^ Type of plan objects, such as lists of actions
                           ptSeed :: Expr  -- ^ Initial plan, such as an empty list, or the identity function
                         }

mcmcPlan :: Expr -> -- ^ Initial plan, such as the empty list, or the identity function
            [(Expr, Double)] -> -- ^ Distribution over expressions drawn from the grammar
            (Expr -> IO Double) -> -- ^ Log Likelihood function
            Int -> -- ^ length of the plan
            IO (Double, [(Expr, Double)], Bool) -- ^ log partition function, expressions and log rewards, hit task
mcmcPlan e0 dist likelihood len =
  mcmc e0 1 0
  where mcmc prefix prefixRecipLike lenSoFar = do
          -- Reweight by the likelihood
          let prefixHasHoles = countHoles prefix > 0
          reweighted <- mapM (\(e, w) -> do
                                 let e' = e <> prefix
                                 like <- if prefixHasHoles || countHoles e > 0
                                         then sampleHoles likelihood 10 e'
                                         else likelihood e'
                                 return ((e, like), like + w)) dist
          -- Failure: all of the likelihoods are zero
          if all (\x -> isNaN x || isInfinite x) (map snd reweighted)
          then return (0, [], False)
          else
             do (e, eLike) <- sampleMultinomialLogProb reweighted
                -- (possibly) recurse
                (suffixLogPartition, suffixRewards, suffixHit) <-
                  if lenSoFar < len - 1
                  then mcmc (e <> prefix) (prefixRecipLike - eLike) (len+1)
                  else return (0, [], False)
                let partitionFunction = logSumExp suffixLogPartition prefixRecipLike
                let epsilon = 0.01 -- tolerance for deciding if a plan has hit a task
                let hit = suffixHit || eLike >= 0.0-epsilon
                return (partitionFunction, (e, partitionFunction):suffixRewards, hit)


doEMPlan :: [PlanTask] -- ^ Tasks
            -> (PlanTask -> PlanTask -> Bool) -- ^ Partial order among tasks: is the first task harder than the second task?
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Int -- ^ number of plans sampled per task
            -> Int -- ^ max length of each plan
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
doEMPlan tasks isHarderThan lambda pseudocounts frontierSize numPlans planLen grammar = do
  -- For each type, sample a frontier of actions
  frontiers <- mapM (\tp -> (if sampleByEnumeration then sampleBFM else sampleExprs)
                            frontierSize grammar (tp ->- tp)
                            >>= return . (tp,) . Map.toList)
               $ nub $ map ptType tasks
  putStrLn $ "Frontier size: " ++ (unwords $ map (show . length . snd) frontiers)
  numHitRef <- newIORef 0
  numPartialRef <- newIORef 0
  taskFailures <- newIORef [] -- list of all of the tasks we've failed so far
  -- For each task, do greedy stochastic search to get candidate plans
  -- Each task records all of the programs used in successful plans, as well as the associated rewards
  -- These are normalized to get a distribution over plans, which gives weights for the MDL's of the programs
  normalizedRewards <- forM tasks $ \tsk@PlanTask {ptLogLikelihood = likelihood, ptSeed = seed, ptType = tp, ptName = nm} -> do
    -- Check to see we haven't failed on an easier task.
    -- If so, we can bail on this one.
    previousFailures <- readIORef taskFailures
    if any (isHarderThan tsk) previousFailures
      then putStrLn ("Skipping task " ++ nm) >> return []
      else do
      let frontier = snd $ fromJust $ find ((==tp) . fst) frontiers
      (logPartitionFunction, programLogRewards, anyHit, anyPartial) <- do
        (firstZ,firstRS,firstHit) <- mcmcPlan seed frontier likelihood planLen
        -- If firstResult is empty, (can't make any progress on the task), then just give up
        if null firstRS
          then return (firstZ, [], False, False)
          else do
          planResults <- Parallel.replicateM (numPlans-1) $ mcmcPlan seed frontier likelihood planLen
          return $ foldl (\ (logZ, rewards, hit, part) (logZ', rewards', hit') ->
                           (logSumExp logZ logZ', rewards++rewards', hit||hit', part||(not (null rewards'))))
                         (firstZ, firstRS, firstHit, True) planResults
      when anyHit $ do
        when verbose $ putStrLn $ "Hit " ++ nm
        modifyIORef numHitRef (+1)
      when (not anyPartial) $ modifyIORef taskFailures (tsk:)
      when (verbose && (not anyHit) && (not anyPartial)) $ putStrLn $ "Missed " ++ nm
      when ((not anyHit) && anyPartial) $ do
        when verbose $ putStrLn $ "Got partial credit for " ++ nm
        modifyIORef numPartialRef (+1)
      -- normalize rewards
      return $ map (\(e, r) -> (e, exp (r - logPartitionFunction))) programLogRewards
  numHit <- readIORef numHitRef
  putStrLn $ "Hit " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."
  -- Compress the corpus
  let normalizedRewards' = Map.toList $ Map.fromListWith (+) $ concat normalizedRewards
  let grammar' = compressWeightedCorpus lambda pseudocounts grammar normalizedRewards'
  let terminalLen = length $ filter isTerm $ Map.keys $ grExprDistr grammar
  putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - terminalLen - 1) ++ " new productions."
  putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar') ++ " nats."
  when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'
  putStrLn "" -- newline
  return grammar'
