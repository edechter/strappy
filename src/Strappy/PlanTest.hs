
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

makePolyTask :: Int -> Int -> Int -> PlanTask
makePolyTask a b c = PlanTask { ptName = show a ++ "x^2 + " ++ show b ++ "x + " ++ show c,
                                   ptLogLikelihood =
                                     (\poly -> - (sse poly (\x -> a * x * x + b * x + c))),
                                   ptType = tInt ->- tInt,
                                   ptSeed = cI }

sse :: Expr -> (Int -> Int) -> Double
sse poly correct =
  let square z = z*z
      polyVals    = map (\x -> eval (poly <> cInt2Expr x)) [1..9]
      correctVals = map correct [1..9]
  in fromIntegral $ sum $ zipWith (\w v -> square (w-v)) polyVals correctVals

polyPlan :: IO ()
polyPlan = do
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- polyExprs ] }
  let tasks = [ makePolyTask a b c | a <- [0..5], b <- [0..5], c <- [0..5] ]
  loopM seed [0..14] $ \grammar step -> do
    putStrLn $ "EM Planning Iteration: " ++ show step
    grammar' <- doEMPlan tasks 1.5 1.0 frontierSize numberOfPlansPerTask maximumPlanLength grammar
    return grammar'
  return ()


main = polyPlan