-- | Uses the planner to build tall, stable towers

module Main where

import Strappy.Planner
import Strappy.Type
import Strappy.Expr
import Strappy.Library
import Strappy.Config
import Strappy.Utils
import Strappy.EM
import Strappy.BeamSearch

import Physics.PhysicsCache
import Physics.BlockClient

import Data.List
import qualified Data.Map as Map
import System.IO.Unsafe
import Data.Maybe
import Debug.Trace
import Control.Monad

import System.Environment
import System.Random

makeTowerTask :: SharedCache -> PlanTask
makeTowerTask cache =
  PlanTask { ptName = "TowerTask",
             ptType = tList (tPair tDouble tBool),
             ptSeed = cEmpty,
             ptLogLikelihood = towerLikelihood cache
           }

towerLikelihood :: SharedCache -> Expr -> IO Double
towerLikelihood cache e =
  case timeLimitedEval e of
    Nothing -> return (log 0)
    Just plan -> planLikelihood cache plan

planLikelihood :: SharedCache -> [(Double, Bool)] -> IO Double
planLikelihood _ plan | length plan > 8 = return (log 0)
planLikelihood cache plan = do
  (ht, stabilities) <- cachedPerturb cache [0.9, 1.1] plan
  let ll = ((ht - gnd_height) * (sum stabilities) + (sum stabilities) {- - log (genericLength plan)-})/3.0
  if isNaN ht || isInfinite ht
    then return (log 0)
    else return $ trace ("LL: " ++ show ll ++ "\n\t" ++ show plan) ll

main = do
  cache <- loadPhysicsCache "physics_cache"
  let seed = Grammar { grApp = log 0.45,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- towerExprs ] }
  let compileTower :: Expr -> Maybe [(Double, Bool)]
      compileTower expr = timeLimitedEval expr
  let task = makeTowerTask cache
  let emtask = EMTask { etName = "tower",
                        etLogLikelihood = unsafePerformIO . towerLikelihood cache,
                        etType = tList (tPair tDouble tBool) }
  [rndSeed, planOrEm, lambda, pseudocounts, fSize, beamWidth, maxPlanLen, prefix] <- getArgs
  let planning = head planOrEm == 'p'
  setStdGen $ mkStdGen $ read rndSeed
  loopM seed [1..10] $ \grammar step -> do
    if planning
       then putStrLn $ "EM Planning Iteration: " ++ show step
       else putStrLn $ "EM Iteration: " ++ show step
    grammar' <- if planning
                   then liftM fst $ doEMBeam (Just $ prefix ++ "/best_" ++ show step) [task]
                                             (read lambda) (read pseudocounts) (read fSize) (read beamWidth) (read maxPlanLen) grammar
                   else doEMIter (prefix ++ "/best_" ++ show step) [emtask]
                                 (read lambda) (read pseudocounts) (read fSize) grammar
    saveGrammar (prefix ++ "/grammar_"++show step) grammar'
    savePhysicsCache cache "physics_cache"
    return grammar'
