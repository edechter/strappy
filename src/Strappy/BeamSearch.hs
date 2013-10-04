{-# LANGUAGE TupleSections  #-}


module Strappy.BeamSearch where

import Strappy.Planner
import Strappy.Utils
import Strappy.Expr
import Strappy.Type
import Strappy.LPCompress
import Strappy.Sample
import Strappy.Library
import Strappy.Config

import qualified Data.PQueue.Min as MinQueue
import Control.Monad.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.IORef
import Data.Maybe
import Data.List

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
			[(Expr, Double)] -> -- ^ (Log) Distribution over expressions drawn from the grammar
            Int -> -- ^ length of the plan
			IO (Map.Map Expr Double, Set.Set ([Expr], Double)) -- ^ Log rewards for each expression, programs hitting task
beamPlan pt beamSize dist planLen = do
    (logRewards, logZ, hits) <- bs [([], 0.0)] planLen
    return (Map.map (\x -> x-logZ) logRewards, hits)
    where bs :: [([Expr], Double)] -> -- ^ Incoming plans to be extended
				Int -> -- ^ Remaining plan length
				IO (Map.Map Expr Double, Double, Set.Set ([Expr], Double)) -- ^ Log rewards, log partition function, hits
          bs _ 0 = return (Map.empty, 0.0, Set.empty)
          bs partialPlans pLen = do
		  	-- Obtain new plans
		  	let newPlans = [ (e:p, eLL+pLL) | (p, pLL) <- partialPlans, (e, eLL) <- dist ]
		  	-- Score those plans
		  	scoredPlans <- mapM (\(p, pLL) -> ptLogLikelihood pt (foldr1 (<>) $ p++[ptSeed pt]) >>= \ll ->
		  									  return (ll+pLL, (p, pLL))) newPlans
		  	-- See if any of the plans hit the task
		  	let myHits = Set.fromList [ (p, pLL) | (a, (p, pLL)) <- scoredPlans,
		  											a-pLL > -0.1 ]
		  	-- Calculate contribution to log Z from these new plans
		  	let myLogZ = logSumExpList $ map fst scoredPlans
		  	-- Calculate contribution to rewards
		  	let myRewards = Map.fromListWith logSumExp $
		  					concatMap (\(logReward, (p, _)) -> map (,logReward) p) scoredPlans
		  	-- Calculate new beam
		  	let newBeam =
		  		map snd $ MinQueue.toList $
		  		foldl (\beam (planScore, plan) -> insertIntoBeam beamSize beam planScore plan) MinQueue.empty scoredPlans
		  	-- Recurse
		  	(laterRewards, laterZ, laterHits) <- bs newBeam (pLen-1)
		  	return (Map.unionWith logSumExp laterRewards myRewards,
		  			logSumExp laterZ myLogZ,
		  			Set.union laterHits myHits)

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
  frontiers <- mapM (\tp -> (if sampleByEnumeration then sampleBitsM else sampleExprs)
                            frontierSize grammar (tp ->- tp)
                            >>= return . (tp,) . Map.toList)
               $ nub $ map ptType tasks
  putStrLn $ "Frontier sizes: " ++ (unwords $ map (show . length . snd) frontiers)
  numHitRef <- newIORef 0
  numPartialRef <- newIORef 0
  
  -- Each task records all of the programs used in successful plans, as well as the associated rewards
  -- These are normalized to get a distribution over plans, which gives weights for the MDL's of the programs
  aggregateRewards <- forM tasks $ \tsk -> do
    let frontier = snd $ fromJust $ find ((==(ptType tsk)) . fst) frontiers
    (rewards, hits) <- beamPlan tsk beamSize frontier planLen
    let anyHit = not (Set.null hits)
    when anyHit $ do
      when verbose $ putStrLn $ "Hit " ++ (ptName tsk)
      modifyIORef numHitRef (+1)
    when (verbose && (not anyHit)) $ putStrLn $ "Missed " ++ (ptName tsk)
    return $ Map.toList rewards
  
  numHit <- readIORef numHitRef
  putStrLn $ "Hit " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."
  
  -- Save out the best program for each task
  -- TODO
  -- maybe (return ()) (\fname -> saveBestPlan fname $ zip planResults $ map ptName tasks) maybeFname

  -- Compress the corpus
  let rewards = Map.toList $ Map.map exp $ Map.fromListWith logSumExp $ concat aggregateRewards
  let grammar' = compressWeightedCorpus lambda pseudocounts grammar rewards
  let terminalLen = length $ filter isTerm $ Map.keys $ grExprDistr grammar
  putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - terminalLen - 1) ++ " new productions."
  putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ Map.elems $ grExprDistr grammar') ++ " nats."
  when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'
  putStrLn "" -- newline
  return (grammar', numHit)