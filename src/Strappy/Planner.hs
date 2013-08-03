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
import Data.Function


data PlanTask = PlanTask { ptName :: String,
                           ptLogLikelihood :: Expr -> IO Double,
                           ptType :: Type, -- ^ Type of plan objects, such as lists of actions
                           ptSeed :: Expr     -- ^ Initial plan, such as an empty list, or the identity function
                         }

mcmcPlan :: Expr -> -- ^ Initial plan, such as the empty list, or the identity function
            [(Expr, Double)] -> -- ^ Distribution over expressions drawn from the grammar
            (Expr -> IO Double) -> -- ^ Log Likelihood function
            Int -> -- ^ length of the plan
            [Double] -> -- ^ Random stream in range [0,1) with length >= length of plan
            IO (Double, [(Expr, Double)],
                Bool, [([Expr], Double, Double)]) -- ^ log partition function, expressions and log rewards,
                                                  -- ^ hit task, programs tried (es, p(.|g), p(t|.))
mcmcPlan e0 dist likelihood len randstream =
  mcmc e0 [] 0 1 0 randstream
  where mcmc :: Expr -> [Expr] -> Double -> Double -> Int -> [Double] -> IO (Double, [(Expr, Double)], Bool, [([Expr], Double, Double)])
        mcmc prefix prefixList prefixLL prefixRecipLike lenSoFar randNums = do
          -- Reweight by the likelihood
          let prefixHasHoles = countHoles prefix > 0
          reweighted <- mapM (\(e, w) -> do
                                 let e' = e <> prefix
                                 like <- if prefixHasHoles || countHoles e > 0
                                         then expectedLikelihood likelihood 10 e'
                                         else likelihood e'
                                 return ((e, like, w), like + w)) dist
          -- Record all of the new programs explored
          let newPrograms = map (\((e, ll, w), llPlusW) ->
                                  (e : prefixList, w + prefixLL, ll)) reweighted
          -- Failure: all of the likelihoods are zero
          if all (\x -> isNaN x || isInfinite x) (map snd reweighted)
          then return (0, [], False, [])
          else
             do let (e, eLike, eW) = sampleMultinomialLogProbNogen reweighted (head randNums)
                -- (possibly) recurse
                (suffixLogPartition, suffixRewards, suffixHit, suffixNewPrograms) <-
                  if lenSoFar < len - 1
                  then mcmc (e <> prefix) (e:prefixList) (eW+prefixLL) (prefixRecipLike - eLike) (len+1) (tail randNums)
                  else return (0, [], False, [])
                let partitionFunction = logSumExp suffixLogPartition prefixRecipLike
                let epsilon = 0.01 -- tolerance for deciding if a plan has hit a task
                let hit = suffixHit || eLike >= 0.0-epsilon
                return (partitionFunction, (e, partitionFunction):suffixRewards, hit, nub $ newPrograms++suffixNewPrograms)


{-doEMPlan :: Eq a, Ord a, Show a =>
            [PlanTask a] -- ^ Tasks
            -> (PlanTask a -> PlanTask a -> Bool) -- ^ Partial order among tasks: is the first task harder than the second task?
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Int -- ^ number of plans sampled per task
            -> Int -- ^ max length of each plan
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
-}
doEMPlan fname tasks isHarderThan lambda pseudocounts frontierSize numPlans planLen grammar = do
  -- For each type, sample a frontier of actions
  frontiers <- mapM (\tp -> (if sampleByEnumeration then sampleBitsM else sampleExprs)
                            frontierSize grammar (tp ->- tp)
                            >>= return . (tp,) . Map.toList)
               $ nub $ map ptType tasks
  putStrLn $ "Frontier size: " ++ (unwords $ map (show . length . snd) frontiers)
  numHitRef <- newIORef 0
  numPartialRef <- newIORef 0
  taskFailures <- newIORef [] -- list of all of the tasks we've failed so far
  programsPerTask <- newIORef []
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
      (logPartitionFunction, programLogRewards, anyHit, anyPartial, newProgs) <- do
        rnds <- replicateM numPlans $ replicateM planLen $ getRandomR (0, 1)
        planResults <- flip Parallel.mapM rnds $ mcmcPlan seed frontier likelihood planLen
        return $ foldl (\ (logZ, rewards, hit, part, newProgs) (logZ', rewards', hit', newProgs') ->
                         (logSumExp logZ logZ', rewards++rewards', hit||hit', part||(not (null rewards')), nub $ newProgs++newProgs'))
                       (log 0, [], False, False, []) planResults
      when anyHit $ do
        when verbose $ putStrLn $ "Hit " ++ nm
        modifyIORef numHitRef (+1)
      when (not anyPartial) $ modifyIORef taskFailures (tsk:)
      when (verbose && (not anyHit) && (not anyPartial)) $ putStrLn $ "Missed " ++ nm
      when ((not anyHit) && anyPartial) $ do
        when verbose $ putStrLn $ "Got partial credit for " ++ nm
        modifyIORef numPartialRef (+1)
      modifyIORef programsPerTask $ \progs -> progs ++ [(nm, newProgs)]
      -- normalize rewards
      return $ map (\(e, r) -> (e, exp (r - logPartitionFunction))) programLogRewards
  numHit <- readIORef numHitRef
  putStrLn $ "Hit " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."
  -- Show number of unique programs
  numUnique <- flip liftM (readIORef programsPerTask) $ \progs ->
    length $ nub $ map (\(x,y,z)->x) $ concat $ map snd progs
  putStrLn $ "# unique programs tried: " ++ show numUnique
  -- Save out the best program for each task
  readIORef programsPerTask >>= saveBestPlan fname
  -- Compress the corpus
  let normalizedRewards' = Map.toList $ Map.fromListWith (+) $ concat normalizedRewards
  let grammar' = compressWeightedCorpus lambda pseudocounts grammar normalizedRewards'
  let terminalLen = length $ filter isTerm $ Map.keys $ grExprDistr grammar
  putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - terminalLen - 1) ++ " new productions."
  putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar') ++ " nats."
  when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'
  putStrLn "" -- newline
  return grammar'


saveBestPlan :: String -> [(String, [([Expr], Double, Double)])] -> IO ()
saveBestPlan fname plans =
  writeFile fname $ unlines $ flip map plans $ \(nm, progs) ->
  if null progs
  then ("Missed " ++ nm)
  else let compareProgs (_, w, ll) (_, w', ll') =
             case compare ll ll' of
               EQ -> compare w w'
               c -> c
           (bestPlan, bestW, bestLL) = maximumBy compareProgs progs
       in nm ++ "\t" ++ show bestPlan ++ "\t" ++ show bestLL