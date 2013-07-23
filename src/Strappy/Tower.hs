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

makeTowerTask :: SharedCache -> PlanTask
makeTowerTask cache =
  PlanTask { ptName = "TowerTask",
             ptType = tList (tPair tDouble tBool),
             ptSeed = mkTerm "[]" (tList (tPair tDouble tBool)) [],
             ptLogLikelihood =
               \plan -> do
                 case timeLimitedEval plan of
                   Nothing -> return (log 0)
                   Just [] -> return (log 0)
                   Just p | length p > 6 -> return (log 0)
                   Just plan' -> do
                     (ht, stabilities) <- cachedPerturb cache [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9] plan'
                     if isNaN ht || isInfinite ht
                       then return (log 0)
                       else return $ -- Magic formula for the log likelihood of a tower:
                            0.5 * sum stabilities + (1.7 * ht) - 0.3 * log (genericLength plan')
           }

main = do
  cache <- loadPhysicsCache "physics_cache"
  let seed = Grammar { grApp = log 0.45,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- towerExprs ] }
  let num = 0
  let task = makeTowerTask cache
--  (seed, num) <- loadNextGrammar -- Replace with above commented out code to start fresh
  loopM seed [num+1..num+21] $ \grammar step -> do
    putStrLn $ "EM Planning Iteration: " ++ show step
    grammar' <- doEMPlan [task] (\x y -> False) 0.03 0.06 frontierSize numberOfPlansPerTask maximumPlanLength grammar
    saveGrammar ("grammar_"++show step) grammar'
    savePhysicsCache cache "physics_cache"
    return grammar'
