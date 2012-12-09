-- Test.hs
{-# Language ParallelListComp #-}

module Main where

import qualified Data.Map as Map

import Type
import CL
import Expr
import StdLib
import Enumerate
import Task
import Search
import qualified CombTrie as CT
import Experiment
import qualified Compress as CP

-- xs = fst 
--      $ runSearch 
--      $ findCombinatorsForEachDatum expIntegerSequences stdlibTrie 
-- xs' = filter (\x -> (length $ snd x) > 0)  xs
-- (index, rs) = greedyN 5 (sortData xs')
-- index' = newLibrary $ map snd rs

index = fst $ runSearch $ loop expIntegerSequences
main = do
--   let lib = stdlibTrie
--       c = (map runTI $ enumCombsToProb lib (-10) 3 Rtype)
--   putStrLn $ show $ map ((reduceWithLimit rlimit . comb2Expr') . fst) c

--   putStrLn "Results: "
--   let results = loop expIntegerSequences
--       ks = CT.keys results
--       vs = CT.toList results
--       out1 = [ show v ++ ": " ++ show' k | k <- ks | v <- vs]
--   putStrLn $ unlines out1
       
   putStrLn $ unlines $ map (\(c, n) -> show' c ++ ": " ++ show n) (CT.toAscList index)
--   putStrLn $ unlines $ map (\(c, n) -> show' n ++ ": " ++ show c) rs
--   putStrLn $ unlines $ map (\(c, n) -> show' c ++ ": " ++ show n) (CT.toAscList index')