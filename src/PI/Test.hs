-- Test.hs
{-# Language ParallelListComp #-}

module Main where

import qualified Data.Map as Map
import Control.Monad.State

import Type
import CL
import Expr
import StdLib
import Enumerate
import Task
import Search
import qualified CombMap as CM
import Experiment
import qualified Compress as CP
import ListExperiment

-- xs = fst 
--      $ runSearch 
--      $ findCombinatorsForEachDatum expIntegerSequences stdlibTrie 
-- xs' = filter (\x -> (length $ snd x) > 0)  xs
-- (index, rs) = greedyN 5 (sortData xs')
-- index' = newLibrary $ map snd rs

grammar = fst $ runSearch $ loop expTList1
--y = map reduceComb$ map fst $ runStateT (enum (CM.keys stdlib) 3 tInt) 0
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
   putStrLn $ show grammar    
--   putStrLn $ unlines $ map (\(c, n) -> show' c ++ ": " ++ show n) (CM.assocs index)
--   putStrLn $ unlines $ map (\(c, n) -> show' n ++ ": " ++ show c) rs
--   putStrLn $ unlines $ map (\(c, n) -> show' c ++ ": " ++ show n) (CT.toAscList index')

--putStrLn $ show y