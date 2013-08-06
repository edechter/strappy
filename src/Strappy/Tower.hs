-- | Uses the planner to build tall, stable towers

module Main where

import Strappy.Planner
import Strappy.Type
import Strappy.Expr
import Strappy.Library
import Strappy.Config
import Strappy.Utils
import Strappy.EM

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

makeReflectTask :: PlanTask
makeReflectTask =
  PlanTask { ptName = "ReflectTask",
             ptType = (tList (tPair tDouble tBool)) ->- (tList (tPair tDouble tBool)),
             ptSeed = cI,
             ptLogLikelihood = \e -> let ts = [[(0.5, True),(0.5, False)],
                                               [(1.0, True),(0.0, False),(0.0, True)],
                                               [(1.0, True),(0.3, False),(0.0, True),(1.0, False),(3.0, False),(-1.0, True)]]
                                         rs = map (map (\(x,o) -> (-x,o))) ts
                                         eTs = mapM (\t -> timeLimitedEval $ e <> mkTerm "notahole" undefined t) ts
                                     in case eTs of
                                       Just eTs' | eTs' == rs -> return 0.0
                                       _ -> return (log 0.0)
           }
makeFlipTask :: PlanTask
makeFlipTask =
  PlanTask { ptName = "FlipTask",
             ptType = (tList (tPair tDouble tBool)) ->- (tList (tPair tDouble tBool)),
             ptSeed = cI,
             ptLogLikelihood = \e -> let ts = [[(0.5, True),(0.5, False)],
                                               [(1.0, True),(0.0, False),(0.0, True)],
                                               [(1.0, True),(0.3, False),(0.0, True),(1.0, False),(3.0, False),(-1.0, True)]]
                                         rs = map reverse ts
                                         eTs = mapM (\t -> timeLimitedEval $ e <> mkTerm "notahole" undefined t) ts
                                     in case eTs of
                                       Just eTs' | eTs' == rs -> return 0.0
                                       _ -> return (log 0.0)
           }

towerLikelihood :: SharedCache -> Expr -> IO Double
towerLikelihood cache e =
  case timeLimitedEval e of
    Nothing -> return (log 0)
    Just plan -> planLikelihood cache plan

planLikelihood :: SharedCache -> [(Double, Bool)] -> IO Double
planLikelihood _ plan | length plan > 8 = return (log 0)
planLikelihood cache plan = do
  (ht, stabilities) <- cachedPerturb cache [0.9, 1.3, 1.7] plan
  let ll = ((ht - gnd_height) * (sum stabilities) {- - log (genericLength plan)-})/5.0
  if isNaN ht || isInfinite ht
    then return (log 0)
    else return $ trace ("LL: " ++ show ll ++ "\n\t" ++ show plan) ll

main = do
  cache <- loadPhysicsCache "physics_cache"
  let seed = Grammar { grApp = log 0.45,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- towerExprs ] }
  let compileTower :: Expr -> Maybe [(Double, Bool)]
      compileTower expr = timeLimitedEval expr
  let num = 0
  let task = makeTowerTask cache
  let emtask = (maybe (log 0) (unsafePerformIO . planLikelihood cache), "TowerTask")
--  (seed, num) <- loadNextGrammar -- Replace with above commented out code to start fresh
  loopM seed [num+1..num+21] $ \grammar step -> do
    putStrLn $ "EM Iteration: " ++ show step
    grammar' <- doEMIter ("towerEMlog_" ++ show step) (tList (tPair tBool tDouble)) compileTower [emtask] 0.015 0.1 5000 grammar
--    (grammar',_) <- doEMPlan (Just $ "towerlog_" ++ show step) [task, task, task] 0.015 1.0 frontierSize numberOfPlansPerTask maximumPlanLength grammar
    saveGrammar ("towerEMgrammar_"++show step) grammar'
--    savePhysicsCache cache "physics_cache"
    return grammar'
