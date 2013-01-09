-- Test.hs
{-# Language ParallelListComp #-}

module Main where

import qualified Data.Map as Map
import Control.Monad.State

import Type
import CL
import Expr
import StdLib
import EnumBF
import Task
import Search
import qualified CombMap as CM
import Experiment
import qualified Compress as CP
import ListExperiment

grammar = fst $ runSearch $ loop expIntegerSequences

combs = map comb $ enumBF stdgrammar 1000 (tInt ->- tInt)
main = do
   putStrLn $ show grammar
