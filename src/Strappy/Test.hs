-- Test.hs
{-# Language ParallelListComp #-}

module Main where

import System.IO
import qualified Data.Map as Map
import Control.Monad.State
import Debug.Trace

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
-- import Experiments.ListExperiment
import Experiments.SymbolicRegression
import PostProcess 
import Grammar

out =  runSearch $ loop expSymReg
grammar = (trace $ "OUT: " ++  (show $ snd out)) $ fst out
searchData = snd out

combs = map comb $ enumBF stdgrammar 1000 (tInt ->- tInt)
main = do
  saveSearchData "data" expSymReg searchData
