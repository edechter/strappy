-- ListExperiment.hs

module ListExperiment where 

import Control.Monad
import Debug.Trace

import Expr
import StdLib
import Data
import CL
import CombMap (CombMap)
import Task
import Type
import Experiment


-- Basic List Experiment --


mkIntList :: [Int] -> SynthComb
mkIntList (x:xs) = Right cCons <:> Right (num2C x) <:> mkIntList xs
mkIntList [] = Right cEmpty

maybe2Either :: Maybe a -> Either String a
maybe2Either (Just a) = Right a
maybe2Either Nothing = Left ""

fromRight (Right x) = x

tLast = Task "first" f tp
    where tp = (TAp tList tInt) ->- tInt
          f comb = reward
              where ex = liftM comb2Expr $ Right comb <:> (mkIntList [1,2,3,4]) 
                    result = reduce (fromRight ex)
                    reward = case result of 
                               (N 2) -> 0
                               otherwise -> 1

expTList1 = Experiment {
              expName = "List function: first",
              expTaskSet = [tLast],
              expEps = 0,
              expPrior = stdlib,
              expInitLib = stdlib,
              expDepthBound = 2,
              expDataType = (tInt),
              expReps = 10}
                      
                   