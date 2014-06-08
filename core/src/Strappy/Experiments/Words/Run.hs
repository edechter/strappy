module Main where

import qualified Data.Map as Map

import Strappy.Config
import Strappy.Planner
import Strappy.Utils
import Strappy.Library
import Strappy.Core
import Strappy.Sample
import Strappy.Type
import Strappy.Expr

import Strappy.Experiments.Words.Grammar
import Strappy.Experiments.Words.Tasks
import Strappy.Experiments.Words.Corpus


{-doEMPlan :: Eq a, Ord a, Show a =>
            [PlanTask a] -- ^ Tasks
            -> (PlanTask a -> PlanTask a -> Bool) -- ^ Partial order among tasks: is the first task harder than the second task?
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Int -- ^ number of plans sampled per task
            -> Int -- ^ max length of each plan
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
-}

beta = 3

runWordsEM :: [String] -> IO ()
runWordsEM corpus = do let tasks = map (string2PlanTask beta) corpus 
                           filename = "out_test.txt"
                       loopM wordsGr [0..5] $ \gr step -> do
                        putStrLn $ "EM Planning Iteration: " ++ show step
                        (gr', numHitTasks) <- doEMPlan (Just filename) tasks 2.0 2.0 frontierSize numberOfPlansPerTask maximumPlanLength gr 
                        return gr'
                       return ()

main = 
       do txt <- readFile "alice.txt"
          let tokens = take 100 $ tokenize txt 
          print tokens
          runWordsEM tokens
