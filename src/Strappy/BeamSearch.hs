{-# LANGUAGE TupleSections  #-}


module Strappy.BeamSearch where

import Strappy.Planner
import Strappy.Utils
import Strappy.Expr
import Strappy.Library

import qualified Data.PQueue.Min as MinQueue
import Control.Monad.List
import qualified Data.Map as Map

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
			IO (Map.Map Expr Double) -- ^ Rewards for each expression
beamPlan pt beamSize dist planLen = do
    (logRewards, logZ) <- bs [([], 0.0)] planLen
    return $ Map.map exp $ Map.map (\x -> x-logZ) logRewards
    where bs :: [([Expr], Double)] -> -- ^ Incoming plans to be extended
				Int -> -- ^ Remaining plan length
				IO (Map.Map Expr Double, Double) -- ^ Log rewards, log partition function
          bs _ 0 = return (Map.empty, 0.0)
          bs partialPlans pLen = do
		  	-- Obtain new plans
		  	let newPlans = [ (e:p, eLL+pLL) | (p, pLL) <- partialPlans, (e, eLL) <- dist ]
		  	-- Score those plans
		  	scoredPlans <- mapM (\(p, pLL) -> ptLogLikelihood pt (foldr1 (<>) $ p++[ptSeed pt]) >>= \ll ->
		  									  return (ll+pLL, (p, pLL))) newPlans
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
		  	(laterRewards, laterZ) <- bs newBeam (pLen-1)
		  	return (Map.unionWith logSumExp laterRewards myRewards, logSumExp laterZ myLogZ)

doEMBeam :: Maybe String -> -- ^ Filename to save logs to
            [PlanTask] -> -- ^ Tasks
            Double -> -- ^ Lambda
            Double -> -- ^ pseudocounts
            Int -> -- ^ frontier size
            Int -> -- ^ beam size
            Int -> -- ^ max length of each plan
            Grammar -> -- ^ Initial grammar
            IO (Grammar, Int) -- ^ Improved grammar, # hit tasks
doEMBeam maybeFname tasks lambda pseudocounts frontierSize beamSize planLen grammar = undefined