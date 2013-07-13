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

makeTowerTask :: SharedCache -> Double -> Double -> PlanTask
makeTowerTask cache height stability =
  PlanTask { ptName = "Height: " ++ show height ++ ", " ++ " Stability " ++ show stability,
             ptType = tList (tPair tDouble tBool),
             ptSeed = mkTerm "[]" (tList (tPair tDouble tBool)) [],
             ptLogLikelihood =
               \plan -> unsafePerformIO $ do
                        putStrLn $ show plan
                        case timeLimitedEval plan of
                          Nothing -> return (log 0)
                          Just plan' ->
                            if unsafePerformIO (cachedPerturb cache stability height plan')
                            then return (-0.3 * (genericLength plan'))
                            else return (log 0)
           }

main = do
  cache <- newPhysicsCache
  let tasks = [ makeTowerTask cache height stability | height <- [-3+0.1,
                                                                  0.1,
                                                                  3,
                                                                  6,
                                                                  9,
                                                                  12],
                                                       stability <- [0.05,
                                                                     0.1,
                                                                     0.3,
                                                                     0.5,
                                                                     0.7,
                                                                     0.9,
                                                                     1.2] ]
  let seed = Grammar { grApp = log 0.45,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- towerExprs ] }
  loopM seed [0..20] $ \grammar step -> do
    putStrLn $ "EM Planning Iteration: " ++ show step
    grammar' <- doEMPlan tasks 2.0 2.0 frontierSize numberOfPlansPerTask maximumPlanLength grammar
    saveGrammar ("grammar_"++show step) grammar'
    savePhysicsCache cache "physics_cache"
    return grammar'
