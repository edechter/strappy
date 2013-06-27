
--module PlanTest where
module Main where

import Strappy.Type
import Strappy.Expr
import Strappy.Library
import Strappy.Task
import Strappy.Sample
import Strappy.Utils
import Strappy.Planner
import Strappy.Config

import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Map as Map

makeNumberTask :: Int -> PlanTask
makeNumberTask target = PlanTask { ptName = show target,
                                   ptLogLikelihood =
                                     \x -> - fromIntegral (abs $ (eval x)-target),
                                   ptType = tInt,
                                   ptSeed = cInt2Expr 0 }


numberPlan :: IO ()
numberPlan = do
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- polyExprs ] }
  let tasks = map makeNumberTask [1..50]
  loopM seed [0..14] $ \grammar step -> do
    putStrLn $ "EM Planning Iteration: " ++ show step
    grammar' <- doEMPlan tasks 1.5 1.0 frontierSize numberOfPlansPerTask maximumPlanLength grammar
    return grammar'
  return ()

main = numberPlan