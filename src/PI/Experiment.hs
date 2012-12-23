-- Experiment.hs
{-# Language ParallelListComp #-}

module Experiment where

import Data.Maybe
import Debug.Trace

import Type
import CL
import Expr
import Task
import StdLib
import Data
import qualified CombMap as CM
import  CombMap (CombMap)

data Experiment = Experiment {expName :: String,
                              expTaskSet :: TaskSet,
                              expEps  :: Double,
                              expPrior :: CombMap Int,
                              expInitLib :: CombMap Int,
                              expLogLikeBound :: Double,
                              expDepthBound   :: Int,
                              expDataType :: Type,
                              expReps :: Int}

----- Squared Integers -------
rlimit = 1000

taskSet :: TaskSet
taskSet = map (mkSingleEqualityTask rlimit) xs
    where xs = [i |i <-[1..100]] 

expSquaredInts 
    = Experiment {
        expName = "Squared Integers",
        expTaskSet 
            = taskSet,
        expEps = 0,
        expPrior = stdlib,
        expInitLib = stdlib,
        expLogLikeBound = (-4),
        expDepthBound = 3,
        expDataType = tInt,
        expReps = 10}

------------------------------

---- Integer Sequences ----
rng = [0..10]
red = (reduceWithLimit 1000) . comb2Expr
eval :: [Comb] -> Comb -> Double
eval ds c = fromIntegral $ sum $ [(toInteger $ diff i) | i <- rng]
         where diff i = abs $ dat i - eval i
               eval i = if y < 0 then 2147483647 else y 
                   where Just (N y) 
                             = let x = red  (CApp c (num2C i) tInt 0) 
                               in x
                                             
               dat i = y where Just (N y) = red (ds !! i)

quad a b c = \i -> a * i^2 + b * i + c
showQuad a b c = show a ++ " x^2 + " ++ show b ++ " x + " ++ show c
mkQuadTask a b c = Task (showQuad a b c) f (tInt ->- tInt)
    where f = eval [num2C $ (quad a b c i)| i<-rng]

squares a b = \i -> (a * i + b)^2
showSquares a b = "(" ++ show a ++ " x + " ++ show b ++ ")^2"
mkSquaresTask a b = Task (showSquares a b) f (tInt ->- tInt)
                   
    where f = eval [num2C $ (squares a b) i| i<-rng]

taskSet' = --[mkSquaresTask i j | i <- [0..10] , j <- [0..10]]
--           ++ [mkQuadTask i j k | i <- [0..10] , j <- [0..10], k <- [0..10]]
            map (mkSingleEqualityTask 10000) $ [0..10] ++ [2*i | i <- [0..10]]
                                    
expIntegerSequences 
    = Experiment {
        expName = "Integer Sequences", 
        expTaskSet = taskSet',
        expEps = 0,
        expPrior = stdlib,
        expInitLib = stdlib,
        expLogLikeBound = (-6),
        expDepthBound = 3,
        expDataType = (tInt ->- tInt),
        expReps=40
      }
 

----- List Functions ----------
-- builtins
-- (++), (:), tail, map, foldr, foldl
-- 1) length
-- 2) reverse
-- 3) concat
-- 4) isPalindrome
-- 5) flatten
-- 6) compress (eliminate consecutive duplicates)
-- 7) pack (pack consecutive elements of the same type into sublists)
-- 8) duplicate (duplicate the elements of a list)
-- 9) replicate (replicate the elements of a list N times)
-- 10) drop (drop every Nth element of a list)
-- 11) split ... a list into two parts with the length of the first part given
-- 12) slice ... a list from the first index to the second

-------------------------

-- cCONS = CNode ":" (Func $ \(R r) -> Func $ \(IntList rs) -> IntList (r:rs)) tp
--         where tp = Map Rtype (Map TyIntList TyIntList)

-- cEmpty = CNode "[]" (IntList []) tp
--          where tp = TyIntList


        
    