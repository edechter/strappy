-- DigitalArithmetic

module Experiments.DigitalArithmetic where

import Control.Monad.State
import Data.Maybe
import Debug.Trace

import Experiment
import Type

import Type
import CL
import Search
import Expr
import Task
import StdLib
import Data
import qualified CombMap as CM
import CombMap (CombMap)
import Grammar
import Routers 
import PostProcess
import EnumBF
import ParseCL hiding (eval)
import Run

numto1dig n = n `mod` 2 
numto2dig n = let a = n `div` 1 `mod` 2
                  b = (n-a*1) `div` 2 `mod` 2
              in (b, a)
numto3dig n = let a = n `div` 1 `mod` 2
                  b = (n-a * 1) `div` 2 `mod` 2
                  c = (n-a * 1 - b*2) `div` 4 `mod` 2
              in (c, b, a)
numto4dig n = let a = n `div` 1 `mod` 2
                  b = (n-a * 1) `div` 2 `mod` 2
                  c = (n-a * 1 - b*2) `div` 4 `mod` 2
                  d = (n-a * 1 - b*2 - c*4) `div` 16 `mod` 2
              in (d, c, b, a)
numto5dig n = let a = n `div` 1 `mod` 2
                  b = (n-a * 1) `div` 2 `mod` 2
                  c = (n-a * 1 - b*2) `div` 4 `mod` 2
                  d = (n-a * 1 - b*2 - c*4) `div` 16 `mod` 2
                  e = (n-a * 1 - b*2 - c*4 - d*16) `div` 32 `mod` 2
              in (e, d, c, b, a)

intToBool 1 = True
intToBool 0 = False
intToBool i = error $ "Cannot convert int " ++ show i ++ "to bool."

intTupleToPair :: (Int, Int) -> Comb
intTupleToPair (i, j) = case parseExprStd ( "pair " ++ show (intToBool i) ++ " " 
                                            ++ show (intToBool j)) of
                          Right x -> x
                          Left _  -> error "Cannot convert in to pair."


intTupleToTriple :: (Int, Int, Int) -> Comb
intTupleToTriple (i, j, k) = case parseExprStd ( "triple " ++ show (intToBool i) ++ " " 
                                               ++ show (intToBool j) ++ " " 
                                               ++ show (intToBool k)) of
                          Right x -> x
                          Left err  -> error err

pairEq p1 p2 = let eq1 = reduceComb (app' cFst p1) == reduceComb (app' cFst p2)
                   eq2 = reduceComb (app' cSnd p1) == reduceComb (app' cSnd p2)
               in eq1 && eq2
                   
               

taskIncr1 = Task "pair incr 1"
            f
            ((TAp tPair tBool) ->- (TAp tPair tBool))
            where f c = if all (==True) [ismatch c i j | (i, j) <- points] then 0 else 1
                  pairIn0 = intTupleToPair (0, 0)
                  pairOut0 = intTupleToPair (0, 1)
                  pairIn1 = intTupleToPair (0, 1)
                  pairOut1 = intTupleToPair (0, 0)
                  pairIn2 = intTupleToPair (1, 0)
                  pairOut2 = intTupleToPair (1, 1)
                  points = [(pairIn0, pairOut0),
                            (pairIn1, pairOut1),
                            (pairIn2, pairOut2)]
                  ismatch c cin cout = pairEq (app' c cin) cout


lib = (CM.fromList $ 
         [
          ("I", cI)
        , ("True", cTrue)
        , ("False", cFalse)
         ,("nand", cNand)
--         , ("&", cAnd)
--         , ("not", cNot)
        , ("pair", cPair)
        , ("fst", cFst)
        , ("snd", cSnd)
--         , ("triple", cTriple)
--         , ("fst3", cFst3)
--         , ("snd3", cSnd3)
--         , ("thrd3", cThrd3)
           ]) 

lib1 = lib `CM.union` one_routers

grammarFromLib lib = normalizeGrammar $ Grammar l 0
    where l = CM.fromList $ [(c, 0) | c <- CM.elems lib]

expDigArith = Experiment {expName = "dig arith",
                          expTaskSet = [taskIncr1],
                          expEps = 0,
                          expPrior = grammarFromLib lib1,
                          expInitLib = grammarFromLib lib1,
                          expDepthBound=3,
                          expNumBound = 5000,
                          expReps = 1}

main = do
  runExp expDigArith






