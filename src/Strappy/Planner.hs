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
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.IORef
import Data.Function
import Debug.Trace


data PlanTask = PlanTask { ptName :: String,
                           ptLogLikelihood :: Expr -> IO Double,
                           ptType :: Type, -- ^ Type of plan objects, such as lists of actions
                           ptSeed :: Expr     -- ^ Initial plan, such as an empty list, or the identity function
                         }

data PlanResult = PlanResult { prRewards :: Map.Map Expr Double, -- Log rewards
                               prUniqueProgs :: Set.Set Expr, -- Set of all programs encountered in this planning iteration
                               prBestPlan :: ([Expr], Double, Double) -- Program, log likelihood, log prior
                             }

mergePlanResults :: PlanResult -> PlanResult -> PlanResult
mergePlanResults (PlanResult { prRewards = rewards,
                               prUniqueProgs = progs,
                               prBestPlan = b@(e, ll, pr) })
                 (PlanResult { prRewards = rewards',
                               prUniqueProgs = progs',
                               prBestPlan = b'@(e', ll', pr') }) =
  PlanResult { prRewards = Map.unionWith logSumExp rewards rewards',
               prUniqueProgs = progs `Set.union` progs',
               prBestPlan =
                 case compare (ll, pr) (ll', pr') of
                   GT -> b
                   _ -> b'
             }

emptyPlanResult :: PlanResult
emptyPlanResult = PlanResult { prRewards = Map.empty,
                               prUniqueProgs = Set.empty,
                               prBestPlan = (undefined, log 0, log 0) }
                               

mcmcPlan :: PlanTask ->
            [(Expr, Double)] -> -- ^ Distribution over expressions drawn from the grammar
            Int -> -- ^ length of the plan
            [Double] -> -- ^ Random stream in range [0,1) with length >= length of plan
            IO PlanResult
mcmcPlan (PlanTask { ptLogLikelihood = likelihood, ptSeed = e0 }) dist len randstream =
  mcmc [e0] 0 0 randstream
  where mcmc :: [Expr] -> -- ^ Plan so far
                Double -> -- ^ Prior under the grammar of the plan so far
                Double -> -- ^ The sum of the log likelihoods of the previous plans
                [Double] -> -- ^ random stream
                IO PlanResult
        mcmc prefix prefixPrior prefixLLs randNums = do
          -- Reweight frontier by likelihoods
          reweighted <- mapM (\(e, w) -> do
                                 let e' = foldr1 (<>) $ e:prefix
                                 like <- expectedLikelihood likelihood 10 e'
                                 return ((e, e', like, w), like + w)) dist
          -- Record new programs
          let newPrograms = Set.fromList $ map (\((_, e, _, _), _) -> e) reweighted
          -- Failure: all of the likelihoods are zero
          if all (isInvalidNum . snd) reweighted
            then return $ emptyPlanResult { prUniqueProgs = newPrograms }
            else do let (e, e', eLike, eW) = sampleMultinomialLogProbNogen reweighted (head randNums)
                    -- (possibly) recurse
                    suffixResults <- if length prefix < len
                                     then mcmc (e:prefix) (eW+prefixPrior) (eLike+prefixLLs) (tail randNums)
                                     else return emptyPlanResult
                    -- Calculate the results for this iteration
                    let (myBestE, _, myBestLL, myBestW) =
                          maximumBy (\ (_, _, ll, w) (_, _, ll', w') -> compare (ll, w) (ll', w')) $
                          map fst reweighted
                    let myBestPlan = (myBestE : prefix, myBestLL, myBestW)
                    let prefix' = reverse $ tail $ reverse prefix -- drop last element of prefix, which is ptSeed
                    let myRewards = Map.fromListWith logSumExp [ (expr, -prefixLLs) | expr <- e:prefix' ]
                    let myResult = PlanResult { prRewards = myRewards,
                                                prUniqueProgs = newPrograms,
                                                prBestPlan = myBestPlan }
                    return $ mergePlanResults myResult suffixResults


doEMPlan :: Maybe String -> -- ^ Filename to save logs to
            [PlanTask] -> -- ^ Tasks
            Double -> -- ^ Lambda
            Double -> -- ^ pseudocounts
            Int -> -- ^ frontier size
            Int -> -- ^ number of plans sampled per task
            Int -> -- ^ max length of each plan
            Grammar -> -- ^ Initial grammar
            IO (Grammar, Int) -- ^ Improved grammar, # hit tasks
doEMPlan maybeFname tasks lambda pseudocounts frontierSize numPlans planLen grammar = do
  -- For each type, sample a frontier of actions
  frontiers <- mapM (\tp -> (if sampleByEnumeration then sampleBitsM else sampleExprs)
                            frontierSize grammar (tp ->- tp)
                            >>= return . (tp,) . Map.toList)
               $ nub $ map ptType tasks
  putStrLn $ "Frontier sizes: " ++ (unwords $ map (show . length . snd) frontiers)
  numHitRef <- newIORef 0
  numPartialRef <- newIORef 0
  
  -- For each task, do greedy stochastic search to get candidate plans
  -- Each task records all of the programs used in successful plans, as well as the associated rewards
  -- These are normalized to get a distribution over plans, which gives weights for the MDL's of the programs
  planResults <- forM tasks $ \tsk -> do
    let frontier = snd $ fromJust $ find ((==(ptType tsk)) . fst) frontiers
    rnds <- replicateM numPlans $ replicateM planLen $ getRandomR (0, 1)
    results <- flip Parallel.mapM rnds $ mcmcPlan tsk frontier planLen
    let planResult = foldl1 mergePlanResults results
    let anyHit = (\(_, ll, _) -> ll >= -0.01) $ prBestPlan planResult
    let anyPartial = (\(_, ll, _) -> not (isInvalidNum ll)) $ prBestPlan planResult
    when anyHit $ do
      when verbose $ putStrLn $ "Hit " ++ (ptName tsk)
      modifyIORef numHitRef (+1)
    when (verbose && (not anyHit) && (not anyPartial)) $ putStrLn $ "Missed " ++ (ptName tsk)
    when ((not anyHit) && anyPartial) $ do
      when verbose $ putStrLn $ "Got partial credit for " ++ (ptName tsk)
      modifyIORef numPartialRef (+1)
    return planResult
  
  numHit <- readIORef numHitRef
  putStrLn $ "Hit " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."
  
  -- Show number of unique programs
  let numUnique = Set.size $ prUniqueProgs $ foldl1 mergePlanResults planResults
  putStrLn $ "# unique programs tried: " ++ show numUnique
  
  -- Save out the best program for each task
  maybe (return ()) (\fname -> saveBestPlan fname $ zip planResults $ map ptName tasks) maybeFname
  
  -- Normalize the frontiers for each task
  let normalizedRewards = map (\pr -> normalizeDist $ Map.toList $ prRewards pr) planResults
  
  -- Compress the corpus
  let normalizedRewards' = Map.toList $ Map.map exp $ Map.fromListWith logSumExp $ concat normalizedRewards
  let grammar' = compressWeightedCorpus lambda pseudocounts grammar normalizedRewards'
  let terminalLen = length $ filter isTerm $ Map.keys $ grExprDistr grammar
  putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - terminalLen - 1) ++ " new productions."
  putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar') ++ " nats."
  when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'
  putStrLn "" -- newline
  return (grammar', numHit)


saveBestPlan :: String -> [(PlanResult, String)] -> IO ()
saveBestPlan fname plans =
  writeFile fname $ unlines $ flip map plans $ \(planResult, nm) ->
  let (e, ll, pr) = prBestPlan planResult
  in if isInvalidNum ll
     then nm ++ "\tMissed"
     else nm ++ "\t" ++ show e ++ "\t" ++ show ll
