{-# Language ParallelListComp #-} 

module Experiments.EX3CompareDistributions where

import Control.Monad.Random (fromList)
import Data.List (group, sort)
import Debug.Trace 

import CL
import Type
import BooleanCircuits
import Experiments.CircuitsExperiment
import DigArith
import EnumBF
import PostProcess


combToTruthTable :: Comb -> TruthTable
combToTruthTable c = [(ass, val) | ass <- mkAllAss card | val <- boolVals]
    where (Right card) = fmap ((\x -> x-1) . typeLeftDepth) $ getType c
          boolVals = (trace $ show card) $ evalDigArith card c

getTruthTableDistrFromEnum gr nC = ttCounts
    where cs1 = map comb $ enumBF gr nC (tBool ->- tBool)
          cs2 = map comb $ enumBF gr nC (tBool ->- tBool ->- tBool)
          cs3 = map comb $ enumBF gr nC (tBool ->- tBool ->- tBool ->- tBool)
          tts1 = map combToTruthTable cs1
          tts2 = map combToTruthTable cs2
          tts3 = map combToTruthTable cs3
          tts = tts1 ++ tts2 ++ tts3
          ttCounts = [(map snd (head v), length v) | v <- group (sort tts)]

getTruthTableDistrFromCircuits :: [Circuit] -> [([Bool], Int)]
getTruthTableDistrFromCircuits cs = [(map snd (fromJust $ circuitToTruthTable (head cc)), length cc) | 
                                     cc <- mkEquivClasses cs]
    where fromJust (Just x) = x

showTTCounts :: [([Bool], Int)] -> String
showTTCounts xs = unlines $ map f xs
    where f (vs, i) = renderBool vs ++ " " ++ show i
          renderBool = map (\x -> if (x==True) then '1' else '0')
          

          
main :: IO ()
main = do
  cs <- fmap (fst . unzip) $ readCircuitTasksFile "data/EX3/circuitTasks.txt"
  let  circuitCountList = getTruthTableDistrFromCircuits cs
  gr <- jitteredGrammarFromLib lib1
  let initCombsCountList = getTruthTableDistrFromEnum gr 1000
  grAfter <- getGrammarFromFile lib1 "data/EX2/ex3_circuitlearning_1000_2013-01-28_05-26-02.581677_EST/grammars.csv"
  let afterCombsCountList = getTruthTableDistrFromEnum grAfter 1000             
  writeFile "data/EX3/circuitTTCount.txt" (showTTCounts circuitCountList)
  writeFile "data/EX3/initCombsTTCount.txt" (showTTCounts initCombsCountList)
  writeFile "data/EX3/afterCombsTTCount.txt" (showTTCounts afterCombsCountList)
  return ()
  