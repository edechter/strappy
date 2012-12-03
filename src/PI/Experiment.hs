-- Experiment.hs
{-# Language ParallelListComp #-}

module Experiment where

import Data.Maybe

import Type
import CL
import Expr
import Task
import StdLib
import Data
import qualified CombTrie as CT

data Experiment = Experiment {expName :: String,
                              expTaskSet :: TaskSet,
                              expEps  :: Double,
                              expPrior :: CT.CombTrie Int,
                              expInitLib :: CT.CombTrie Int,
                              expLogLikeBound :: Double,
                              expDepthBound   :: Int,
                              expDataType :: Type,
                              expReps :: Int}

----- Squared Integers -------
rlimit = 1000

taskSet :: TaskSet
taskSet = map (mkSingleEqualityTask rlimit) xs
    where xs = [i*i | i <- [1..20]]   ++ [ i | i <- [1..4]]

expSquaredInts 
    = Experiment {
        expName = "Squared Integers",
        expTaskSet 
            = taskSet,
        expEps = 0,
        expPrior = stdlibTrie,
        expInitLib = stdlibTrie,
        expLogLikeBound = (-4),
        expDepthBound = 3,
        expDataType = Rtype,
        expReps = 100}

------------------------------

---- Integer Sequences ----
rng = [0..10]
red = (reduceWithLimit 1000) . comb2Expr'
eval :: [Comb] -> Comb -> Double
eval ds c = sum $ [diff i | i <- rng]
         where diff i = abs $ dat i - eval i
               eval i = y where Just (R y) = red (CApp c (num2C i) [])
               dat i = y where Just (R y) = red (ds !! (fromIntegral . floor) i)

quad a b c = \i -> a * i^2 + b * i + c
showQuad a b c = show a ++ " x^2 + " ++ show b ++ " x + " ++ show c
mkQuadTask a b c = Task (showQuad a b c)
                   f
    where f = eval [num2C $ (quad a b c i)| i<-rng]

squares a b = \i -> (a * i + b)^2
showSquares a b = "(" ++ show a ++ " x + " ++ show b ++ ")^2"
mkSquaresTask a b = Task (showSquares a b)
                   f
    where f = eval [num2C $ (squares a b) i| i<-rng]

taskSet' = [mkSquaresTask i j | i <- [0..10] , j <- [0..10]]
           ++ [mkQuadTask i j k | i <- [0..10] , j <- [0..10], k <- [0..10]]



                                    
expIntegerSequences 
    = Experiment {
        expName = "Integer Sequences", 
        expTaskSet = taskSet',
        expEps = 0,
        expPrior = stdlibTrie,
        expInitLib = stdlibTrie,
        expLogLikeBound = (-6),
        expDepthBound = 3,
        expDataType = (Map Rtype Rtype),
        expReps=1
      }
        
        
    