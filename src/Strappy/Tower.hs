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

makeTowerTask :: SharedCache -> Double -> Double -> PlanTask
makeTowerTask cache height stability =
  PlanTask { ptName = "Height: " ++ show height ++ ", " ++ " Stability " ++ show stability,
             ptType = tList (tPair tDouble tBool),
             ptSeed = mkTerm "[]" (tList (tPair tDouble tBool)) [],
             ptLogLikelihood =
               \plan -> do
                 case timeLimitedEval plan of
                   Nothing -> return (log 0)
                   Just [] -> return (log 0)
                   Just plan' -> do
                     stayedup <- cachedPerturb cache stability height plan'
                     if stayedup
                       then return (-0.3 * (genericLength plan'))
                       else return (log 0)
           }

isTowerHarderThan :: PlanTask -> PlanTask -> Bool
-- | Is t1 harder than t2?
isTowerHarderThan t1 t2 =
  let n1 = ptName t1
      h1 = (read $ takeWhile (/=',') $ drop 8 n1) :: Double
      s1 = (read $ drop 13 $ dropWhile (/=',') $ drop 8 n1) :: Double
      n2 = ptName t2
      h2 = (read $ takeWhile (/=',') $ drop 8 n2) :: Double
      s2 = (read $ drop 13 $ dropWhile (/=',') $ drop 8 n2) :: Double
  in h1 >= h2 && s1 >= s2

main = do
  cache <- loadPhysicsCache "physics_cache"
  let tasks = [makeTowerTask cache (-3+0.1) 0.1 ] ++
              [ makeTowerTask cache height stability | height <- [0.1,
                                                                  3,
                                                                  6,
                                                                  9],
                                                       stability <- [0.05,
                                                                     0.1,
                                                                     0.3,
                                                                     0.5 ] ]
  let seed = Grammar { grApp = log 0.45,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- towerExprs ] }
  let num = 0
--  (seed, num) <- loadNextGrammar -- Replace with above commented out code to start fresh
  loopM seed [num+1..num+21] $ \grammar step -> do
    putStrLn $ "EM Planning Iteration: " ++ show step
    grammar' <- doEMPlan tasks isTowerHarderThan 1.1 5.0 frontierSize numberOfPlansPerTask maximumPlanLength grammar
    saveGrammar ("grammar_"++show step) grammar'
    savePhysicsCache cache "physics_cache"
    return grammar'
