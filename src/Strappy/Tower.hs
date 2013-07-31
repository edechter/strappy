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
  (ht, stabilities) <- cachedPerturb cache [0.9, 1.3, 1.7, 2.1] plan
  let ll = ((ht - gnd_height) * (sum stabilities) - 0.5 * log (genericLength plan))/5.0
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
    putStrLn $ "EM Iteration: " ++ show step
    grammar' <- doEMIter ("towerem"++show step) (tList (tPair tDouble tBool)) (timeLimitedEval :: Expr -> Maybe [(Double, Bool)])
                         [maybe 0.0 (exp . unsafePerformIO . planLikelihood cache)] 0.01 0.02 frontierSize grammar
--    grammar' <- doEMPlan [makeReflectTask,makeFlipTask,task,task,task] (\x y -> False) 0.015 1.0 frontierSize numberOfPlansPerTask maximumPlanLength grammar
    saveGrammar ("grammar_"++show step) grammar'
    savePhysicsCache cache "physics_cache"
    return grammar'
