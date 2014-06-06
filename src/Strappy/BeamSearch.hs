{-# LANGUAGE TupleSections  #-}


module Strappy.BeamSearch where

import Strappy.Planner
import Strappy.Utils
import Strappy.Expr
import Strappy.Type
import Strappy.Compress
import Strappy.Sample
import Strappy.Library
import Strappy.Config

import qualified Data.PQueue.Min as MinQueue
import qualified Control.Monad.Parallel as Parallel
import Control.Monad.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.IORef
import Data.Maybe
import Data.List
import Data.Function
import Control.DeepSeq (deepseq)

type Beam k a = MinQueue.MinQueue (k, a)

insertIntoBeam :: (Ord k, Ord a) =>
                  Int -> -- ^ Beam size
                  Beam k a -> -- ^ Beam
                  k -> a -> -- ^ New thing
                  Beam k a -- ^ New beam
insertIntoBeam bsize beam key val | MinQueue.size beam < bsize =
  MinQueue.insert (key, val) beam
insertIntoBeam _ beam key val =
  if key > fst (MinQueue.findMin beam) -- We're at least as important as least important member of beam
  then MinQueue.insert (key, val) $ MinQueue.deleteMin beam
  else beam

beamPlan :: PlanTask ->
            Int -> -- ^ Beam size
            [(Expr, Double)] -> -- ^ (Log) Distribution over seed expressions drawn from the grammar
            [(Expr, Double)] -> -- ^ (Log) Distribution over transition expressions drawn from the grammar
            Int -> -- ^ length of the plan
            IO (Map.Map Expr Double, -- ^ Log rewards for each expression
                Set.Set ([Expr], Double), -- ^ programs hitting task
                Maybe ([Expr], Double, Double)) -- ^ Best partial credit
beamPlan pt beamSize seedDist dist planLen = do
    seedRewards <- forM seedDist $ \(e, pLL) -> ptLogLikelihood pt e >>=
                                                \ll -> return (ll, (e, pLL))
    let seedPartial = foldl mergePartial Nothing $ flip map seedRewards $ \(ll, (e, pLL)) ->
                                                                          if isInvalidNum ll
                                                                          then Nothing
                                                                          else Just ([e], ll, pLL)
    let seedPlans = map snd $ MinQueue.toList $
                    foldl (\beam (ll, (e, pLL)) -> insertIntoBeam beamSize beam (ll+pLL, pLL) ([e],pLL)) MinQueue.empty seedRewards
    let seedRewards' = filter (not . isInvalidNum . fst) seedRewards
    let seedRewards'' = Map.fromList $ map (\ (ll, (e, pLL)) -> (e, ll+pLL)) seedRewards'
    let seedLogZ = logSumExpList $ map (\ (ll, (_, pLL)) -> ll + pLL) seedRewards
    let seedHits = Set.fromList [([e], pLL+ll) | (ll, (e, pLL)) <- seedRewards, ll > -0.1 ]
    (logRewards, logZ, hits, part) <- bs seedPlans planLen seedRewards'' seedLogZ seedHits seedPartial
    return (Map.map (\x -> x-logZ) logRewards, hits, part)
    where bs :: [([Expr], Double)] -> -- ^ Incoming plans to be extended
                Int -> -- ^ Remaining plan length
                Map.Map Expr Double -> -- ^ Old log rewards
                Double -> -- ^ Old log Z
                Set.Set ([Expr], Double) -> -- ^ Old hits
                Maybe ([Expr], Double, Double) ->
                IO (Map.Map Expr Double, Double, Set.Set ([Expr], Double), Maybe ([Expr], Double, Double)) -- ^ Log rewards, log partition function, hits
          bs _ 0 rewards logZ hits partial = return (rewards, logZ, hits, partial)
          bs partialPlans pLen oldRewards oldLogZ oldHits oldPartial = do
            -- Obtain new plans
            let newPlans = [ (e:p, eLL+pLL) | (p, pLL) <- partialPlans, (e, eLL) <- dist ]
            -- Score those plans in parallel
            scoredPlans <- {-Parallel.-} mapM (\(p, pLL) -> ptLogLikelihood pt (foldr1 (<>) p) >>= \ll ->
                                                       return (ll+pLL, (p, pLL))) newPlans
            let myPartial = foldl mergePartial oldPartial $ flip map scoredPlans $ \(tLL, (p, pLL)) ->
                                                                          if isInvalidNum tLL
                                                                          then Nothing
                                                                          else Just (p, tLL-pLL, pLL)
            -- See if any of the plans hit the task
            let myHits = Set.fromList [ (p, a) | (a, (p, pLL)) <- scoredPlans,
                                a-pLL > -0.1 ]
            -- Calculate contribution to log Z from these new plans
            let myLogZ = logSumExpList $ map fst scoredPlans
            -- Calculate contribution to rewards
            let myRewards = Map.fromListWith logSumExp $ filter (not . isInvalidNum . snd) $
                    concatMap (\(logReward, (p, _)) -> map (,logReward) p) scoredPlans
            -- Calculate new beam
            let newBeam = map snd $ MinQueue.toList $
                          foldl (\beam (planScore, plan@(_, pLL)) -> insertIntoBeam beamSize beam (planScore, pLL) plan) MinQueue.empty scoredPlans
            -- Merge old with new
            let newLogZ = logSumExp myLogZ oldLogZ
            let newRewards = Map.unionWith logSumExp myRewards oldRewards
            let newHits = Set.union oldHits myHits
            -- A (possibly futile) attempt to reduce memory consumption
            --forceShowHack newRewards
            --forceShowHack newLogZ
            --forceShowHack newHits
            -- Recurse
            bs newBeam (pLen-1) newRewards newLogZ newHits myPartial
          mergePartial Nothing x = x
          mergePartial x Nothing = x
          mergePartial x@(Just (_,ll,pLL)) y@(Just (_,ll',pLL')) =
            if (ll,pLL) > (ll',pLL')
            then x
            else y

doEMBeam :: Maybe String -> -- ^ Filename to save logs to
            [PlanTask] -> -- ^ Tasks
            Double -> -- ^ Lambda
            Double -> -- ^ pseudocounts
            Int -> -- ^ frontier size
            Int -> -- ^ beam size
            Int -> -- ^ max length of each plan
            Grammar -> -- ^ Initial grammar
            IO (Grammar, Int) -- ^ Improved grammar, # hit tasks
doEMBeam maybeFname tasks lambda pseudocounts frontierSize beamSize planLen grammar = do
  -- For each type, sample a frontier of actions
  actionFrontiers <- mapM (\tp -> (if sampleByEnumeration then sampleBitsM else sampleExprs)
                            frontierSize grammar (tp ->- tp)
                            >>= return . (tp,) . Map.toList)
                     $ nub $ map ptType tasks
  seedFrontiers <- mapM (\tp -> (if sampleByEnumeration then sampleBitsM else sampleExprs)
                            frontierSize grammar tp
                            >>= return . (tp,) . Map.toList)
                   $ nub $ map ptType tasks
  putStrLn $ "Action frontier sizes: " ++ (unwords $ map (show . length . snd) actionFrontiers)
  putStrLn $ "Seed frontier sizes: " ++ (unwords $ map (show . length . snd) seedFrontiers)
  numHitRef <- newIORef 0
  numPartialRef <- newIORef 0
  
  -- Each task records all of the programs used in successful plans, as well as the associated rewards
  -- These are normalized to get a distribution over plans, which gives weights for the MDL's of the programs
  aggregateRewards <- loopM Map.empty tasks $ \acc tsk -> do
    let actionFrontier = snd $ fromJust $ find ((==(ptType tsk)) . fst) actionFrontiers
    let seedFrontier = snd $ fromJust $ find ((==(ptType tsk)) . fst) seedFrontiers
    (rewards, hits, maybePartial) <- beamPlan tsk beamSize seedFrontier actionFrontier planLen
    let anyHit = not (Set.null hits)
    when anyHit $ do
      when verbose $ putStrLn $ "Hit " ++ (ptName tsk)
      let (bestPlan, bestLL) = maximumBy (compare `on` snd) (Set.toList hits)
      putStrLn $ show bestPlan ++ "\t\t" ++ show bestLL
      modifyIORef numHitRef (+1)
    when (verbose && (not anyHit)) $
      maybe (putStrLn $ "Missed " ++ (ptName tsk)) (\(p, ll, pLL) ->
                                                     putStrLn $ "Got partial credit for " ++ (ptName tsk)
                                                                ++ "\n" ++ show p ++ "\t" ++ show ll)
                                                    maybePartial
    -- Hack to force Haskell to not use lazy evaluation
    let newAcc = Map.unionWith logSumExp acc rewards
    forceShowHack newAcc
    return newAcc
  
  numHit <- readIORef numHitRef
  putStrLn $ "Hit " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."
  
  -- Save out the best program for each task
  -- TODO
  -- maybe (return ()) (\fname -> saveBestPlan fname $ zip planResults $ map ptName tasks) maybeFname

  -- Compress the corpus
  let rewards = Map.toList $ Map.map exp $ aggregateRewards
  let grammar' = compressWeightedCorpus lambda pseudocounts grammar rewards
  --let grammar' = grammarHillClimb lambda pseudocounts (blankLibrary grammar) frontiers'
  let terminalLen = length $ filter isTerm $ Map.keys $ grExprDistr grammar
  putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - terminalLen - 1) ++ " new productions."
  putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar') ++ " nats."
  when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'
  putStrLn "" -- newline
  return (grammar', numHit)