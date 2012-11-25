-- Test.hs
{-# Language ParallelListComp #-}

module Main where

import qualified Data.Map as Map

import Type
import CL
import Expr
import StdLib
import Enumerate
-- import Search
import qualified CombTrie as CT

-- reps = 100
-- -- dats  = [ R $ i | i <-[0..6]] ++ 
-- --     [ R $ i * j * k | i <- powersOf2, j <- powersOf3, k <- powersOf5]
-- --         where powersOf2 = [2^i | i <- [0..6]]
-- --               powersOf3 = [3^i | i <- [0..6]]
-- --               powersOf5 = [5^i | i <- [0..6]]
-- fac :: Int  -> Double
-- fac n = (map f [0..]) !! n
--         where f 0 = 1
--               f 1 = 1
--               f n = (fromIntegral n) +  fac (n - 1)
-- dats = [(R i) | i <- [0..40]] ++ [(R $ fac i ) | i <- [0..40]] 


-- run :: CT.CombTrie Int
-- run = loop dats stdlibTrie stdlibTrie (-5) 3 100 Rtype reps


main = do
  let lib = Map.elems stdlib
      c = (map runTI $ enumCombsToDepth lib 3 TyIntList)
  putStrLn $ show c
--   putStrLn "Results: "
--   let ks = CT.keys run
--       vs = CT.toList run
--       out = [ show v ++ ": " ++ show' k    | k <- ks | v <- vs]
--   putStrLn $ unlines out
 