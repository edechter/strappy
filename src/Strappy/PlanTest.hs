
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
import Debug.Trace
import Data.Maybe

makePolyTask :: Int -> Int -> Int -> PlanTask
makePolyTask a b c = PlanTask { ptName = show a ++ "x^2 + " ++ show b ++ "x + " ++ show c,
                                ptLogLikelihood =
                                  (\poly -> return $ nsse poly (\x -> a * x * x + b * x + c)),
                                ptType = tInt ->- tInt,
                                ptSeed = cI }

nsse :: Expr -> (Int -> Int) -> Double
nsse poly correct =
  let square z = z*z
      polyVals    = map (\x -> timeLimitedEval (poly <> cInt2Expr x)) [0..9]
      polyVals'   = map fromJust polyVals
      correctVals = map correct [0..9]
  in if all isJust polyVals
     then let ll = fromIntegral $ sum $ zipWith (\w v -> - square (w-v)) polyVals' correctVals
          in if ll > 0.1 -- Integer overflow: this actually happens
             then log 0
             else ll
     else log 0

polyPlan :: IO ()
polyPlan = do
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- polyExprs ] }
  let tasks = [ makePolyTask a b c | a <- [0..9], b <- [0..9], c <- [0..9] ]
  loopM seed [0..5] $ \grammar step -> do
    putStrLn $ "EM Planning Iteration: " ++ show step
    grammar' <- doEMPlan ("polyplan_"++show step) tasks (\_ _ -> False) 1.5 1.0 frontierSize numberOfPlansPerTask maximumPlanLength grammar
    saveGrammar ("polyplan_grammar_"++show step) grammar'
    return grammar'
  return ()


main = polyPlan