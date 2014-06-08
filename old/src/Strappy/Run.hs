-- Runs.hs
{-# Language BangPatterns #-}
module Strappy.Run where

import System.IO
import qualified Data.Map as Map
import Control.Monad.State
import Control.Exception (evaluate)
import Data.Time.Clock
import Debug.Trace

import Strappy.Type
import Strappy.CL
import Strappy.Expr
import Strappy.StdLib
import Strappy.EnumBF
import Strappy.Task
import Strappy.Search
import qualified Strappy.CombMap as CM
import Strappy.Experiment
import qualified Strappy.Compress as CP
import Strappy.PostProcess 
import Strappy.Grammar

runExp :: Experiment -> String -- ^ filename 
       -> IO ()
runExp exp filename = do startTime <- getCurrentTime 
                         let ! (_, !searchData) = runSearch $ loop exp
                         evaluate (last searchData)
                         endTime <- getCurrentTime
                         saveSearchData filename exp searchData (diffUTCTime  endTime startTime)


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




