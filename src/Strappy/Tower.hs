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

makeTowerTask :: SharedCache -> PlanTask [(Double, Bool)]
makeTowerTask cache =
  PlanTask { ptName = "TowerTask",
             ptType = tList (tPair tDouble tBool),
             ptSeed = [],
             ptLogLikelihood =
               \plan -> if length plan > 6
                        then return (log 0)
                        else do
                          (ht, stabilities) <- cachedPerturb cache [0.8, 1.3, 1.7, 2.1] plan
                          if isNaN ht || isInfinite ht
                            then return (log 0)
                            else return $ -- Magic formula for the log likelihood of a tower:
                            (ht - gnd_height) * (sum stabilities) - 0.2 * log (genericLength plan)
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
    grammar' <- doEMPlan [task] (\x y -> False) 0.015 0.03 frontierSize numberOfPlansPerTask maximumPlanLength grammar
    saveGrammar ("grammar_"++show step) grammar'
    savePhysicsCache cache "physics_cache"
    return grammar'
