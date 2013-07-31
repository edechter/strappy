-- | Uses the planner to build tall, stable towers

module Main where

import Strappy.Planner
import Strappy.Type
import Strappy.Expr
import Strappy.Library
import Strappy.Config
import Strappy.Utils

import Physics.PhysicsCache
import Physics.BlockClient

import Data.List
import qualified Data.Map as Map
import System.IO.Unsafe
import Data.Maybe
import Debug.Trace

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
    Just plan | length plan > 6 -> return (log 0)
    Just plan -> do (ht, stabilities) <- cachedPerturb cache [0.8, 1.3, 1.7, 2.1] plan
                    let ll = ((ht - gnd_height) * (sum stabilities) - 0.2 * log (genericLength plan))
                    if isNaN ht || isInfinite ht
                      then return (log 0)
                      else return $ trace ("LL: " ++ show ll ++ "\n\t" ++ show plan) ll

main = do
  cache <- loadPhysicsCache "physics_cache"
  let seed = Grammar { grApp = log 0.45,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- towerExprs ] }
  let num = 0
  let task = makeTowerTask cache
--  (seed, num) <- loadNextGrammar -- Replace with above commented out code to start fresh
  loopM seed [num+1..num+21] $ \grammar step -> do
    putStrLn $ "EM Planning Iteration: " ++ show step
    grammar' <- doEMPlan [task,task,task] (\x y -> False) 0.015 1.0 frontierSize numberOfPlansPerTask maximumPlanLength grammar
    saveGrammar ("grammar_"++show step) grammar'
    savePhysicsCache cache "physics_cache"
    return grammar'
