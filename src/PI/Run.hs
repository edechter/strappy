-- Runs.hs
{-# Language BangPatterns #-}
module Run where

import System.IO
import qualified Data.Map as Map
import Control.Monad.State
import Control.Exception (evaluate)
import Data.Time.Clock
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
runExp exp =   do startTime <- getCurrentTime 
                  let ! (_, !searchData) = runSearch $ loop exp
                  evaluate (last searchData)
                  endTime <- getCurrentTime
                  saveSearchData "data" exp searchData (diffUTCTime  endTime startTime)


mkBruteForceExp :: Experiment -> Experiment
mkBruteForceExp ex = let n = (expNumBound ex) * (expReps ex)
                         name = (expName ex) ++ "_Brute"
                     in ex { expName = name,
                             expNumBound = n,
                             expReps = 1}

runBruteForceExp :: Experiment -> IO ()
runBruteForceExp exp = let expBrute = mkBruteForceExp exp
                           (_, searchData) = runSearch $ loop expBrute
                       in saveSearchData "data" expBrute searchData "nan"




