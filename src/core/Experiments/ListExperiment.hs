-- ListExperiment.hs

module Experiments.ListExperiment where 

import Control.Monad
import Debug.Trace

import Expr
import StdLib
import Data
import CL
import CombMap (CombMap)
import qualified CombMap as CM
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

tLast = Task "last" f tp
    where tp = (TAp tList tInt) ->- (TAp tList tInt)
          f comb = reward
              where ex = liftM comb2Expr $ Right comb <:> (mkIntList [1,2,3,4]) 
                    result = reduce (fromRight ex)
                    reward = case result of 
                               (N 2) -> 0
                               otherwise -> 1
listlib = CM.fromList $ 
          [ ("I", cI)
          , ("S", cS)
          , ("B", cB)
          , ("C", cC)
        , ("cond", cCond)
        , ("True", cTrue)
        , ("False", cFalse)
        , ("|", cOr)
        , ("&", cAnd)
        , ("not", cNot)
--         , ("<", cLT)
--         , (">", cGT)
--         , ("==", cEQ)
          , ("+", dOp2C "+" (+))
--          , ("-", dOp2C "-" (-))
           , ("*", dOp2C "*" (*))
          ,  ("0", num2C 0)
          ,  ("1", num2C 1)
--          , ("2", num2C 2)
--         , ("3", num2C 3)
--         , ("4", num2C 4)
--           , ("primRec", cPrim) 
          , (":", cCons)
          , ("[]", cEmpty)
           , ("head", cHead)
           , ("tail", cTail)
           , ("isEmpty", cIsEmpty)
           ]



expTList1 = Experiment {
              expName = "List function: first",
              expTaskSet = [tLast],
              expEps = 0,
              expPrior = stdgrammar,
              expInitLib = stdgrammar,
              expDepthBound = 2,
              expNumBound = 10,
              expReps = 1}
                      
                   