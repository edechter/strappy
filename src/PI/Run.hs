-- Runs.hs

module Run where

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
import PostProcess 
import Grammar

runExp :: Experiment -> IO ()
runExp exp = let (_, searchData) = runSearch $ loop exp
             in saveSearchData "data" exp searchData

mkBruteForceExp :: Experiment -> Experiment
mkBruteForceExp ex = let n = (expNumBound ex) * (expReps ex)
                         name = (expName ex) ++ "_Brute"
                     in ex { expName = name,
                             expNumBound = n,
                             expReps = 1}

runBruteForceExp :: Experiment -> IO ()
runBruteForceExp exp = let expBrute = mkBruteForceExp exp
                           (_, searchData) = runSearch $ loop expBrute
                       in saveSearchData "data" expBrute searchData




