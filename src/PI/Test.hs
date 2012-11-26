-- Test.hs
{-# Language ParallelListComp #-}

module Main where

import qualified Data.Map as Map

import Type
import CL
import Expr
import StdLib
import Enumerate
import Evaluator
import Search
import qualified CombTrie as CT
import Experiment

main = do
--   let lib = stdlibTrie
--       c = (map runTI $ enumCombsToProb lib (-10) 3 Rtype)
--   putStrLn $ show $ map ((reduceWithLimit rlimit . comb2Expr') . fst) c
  putStrLn "Results: "
  let results = loop expIntegerSequences
      ks = CT.keys results
      vs = CT.toList results
      out = [ show v ++ ": " ++ show' k    | k <- ks | v <- vs]
  putStrLn $ unlines out
 