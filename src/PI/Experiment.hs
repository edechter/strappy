-- Experiment.hs

module Experiment where

import Data.Maybe

import Type
import CL
import Expr
import Evaluator
import StdLib
import Data
import qualified CombTrie as CT

data Experiment = Experiment {expName :: String,
                              expDataSet :: DataSet,
                              expEval :: Evaluator,
                              expEps  :: Double,
                              expPrior :: CT.CombTrie Int,
                              expInitLib :: CT.CombTrie Int,
                              expLogLikeBound :: Double,
                              expDepthBound   :: Int,
                              expDataType :: Type,
                              expReps :: Int}

----- Squared Integers -------
expSquaredInts 
    = Experiment {
        expName = "Squared Integers",
        expDataSet = [[num2C $i*i] | i <- [1..20]]   ++ [[num2C i] | i <- [1..4]],
        expEval = mkEvalSingleEquality 100,
        expEps = 0,
        expPrior = stdlibTrie,
        expInitLib = stdlibTrie,
        expLogLikeBound = (-4),
        expDepthBound = 3,
        expDataType = Rtype,
        expReps = 100}

------------------------------

---- Integer Sequences ----

quad a b c = \i -> a * i^2 + b * i + c
squares a b = \i -> (a * i + b)^2
fs = [quad i j k | i <-[0..5], j<- [0..4], k <-[0..4]]
     ++ [squares i j | i <-[1..4], j <- [0..4]]
rng = [1..10]
red = (reduceWithLimit 1000) . comb2Expr'

g ds c = sum $ [diff i | i <- rng]
         where diff i = abs $ dat i - eval i
               eval i = y where Just (R y) = red (CApp c (num2C i) [])
               dat i = y where Just (R y) = red (ds !! (fromIntegral . floor) i)
                                    
expIntegerSequences 
    = Experiment {
        expName = "Integer Sequences", 
        expDataSet = [[num2C $ f i | i <- [0..10]] | f <- fs],
        expEval = g,
        expEps = 0,
        expPrior = stdlibTrie,
        expInitLib = stdlibTrie,
        expLogLikeBound = (-10),
        expDepthBound = 4,
        expDataType = (Map Rtype Rtype),
        expReps=1
      }
        
        
    