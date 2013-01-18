-- SymbolicRegression.hs
{-# Language ParallelListComp #-}

module Experiments.SymbolicRegression where

import Data.Maybe
import Debug.Trace

import Experiment
import Type

import Type
import CL
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

rng = [0..10]
eval :: [Comb] -> Comb -> Double
eval ds c = fromIntegral $ sum $ [(toInteger $ diff i) | i <- rng]
         where diff i = abs $ dat i - eval i
               eval i = if y < 0 then maxBound else y 
                   where y = case reduceComb $ (CApp c (num2C i) tInt 0) of
                               (N y) -> y
                               otherwise -> maxBound
               dat i = y where (N y) = reduceComb (ds !! i)

quad a b c = \i -> a * i^2 + b * i + c
showQuad a b c = show a ++ " x^2 + " ++ show b ++ " x + " ++ show c
mkQuadTask a b c = Task (showQuad a b c) f (tInt ->- tInt)
    where f = eval [num2C $ (quad a b c i)| i<-rng]

mkQuadSymRegTask a b c = SymRegTask (showQuad a b c) f (tInt ->- tInt)
    where f  = [quad a b c i | i <- [0..10]]

squares a b = \i -> (a * i + b)^2
showSquares a b = "(" ++ show a ++ " x + " ++ show b ++ ")^2"
mkSquaresTask a b = Task (showSquares a b) f (tInt ->- tInt)
                   
    where f = eval [num2C $ (squares a b) i| i<-rng]

taskSet' = --[mkSquaresTask i j | i <- [0..5] , j <- [0..5]]
           -- ++  (map (mkSingleEqualityTask 100) $ [i*i | i <- [0..20]])
           [mkQuadTask i j k | i <- [0..10] , j <- [0..10], k <- [0..10]]
--           ++ [mkQuadTask i j k | i <- [0..0] , j <- [0..5], k <- [0..5]]

symRegTaskSet' = --[mkSquaresTask i j | i <- [0..5] , j <- [0..5]]
           -- ++  (map (mkSingleEqualityTask 100) $ [i*i | i <- [0..20]])
           [mkQuadSymRegTask i j k | i <- [0..10] , j <- [0..10], k <- [0..10]]

cLin = CNode "Lin" (Func $ \(N a) -> Func $ \(N b) -> Func $ \(N x) 
                              -> (N $ a * x + b )) typeC 
                    where typeC = tInt ->- tInt ->- tInt ->- tInt


lib = CM.fromList $ 
          [ 
           ("I", cI)
--          , ("K", cK)
--           ,("SSS", cSSS)
--          , ("cond", cCond)
--          , ("True", cTrue)
--          , ("False", cFalse)
--          , ("|", cOr)
--          , ("&", cAnd)
--          , ("not", cNot)
--         , ("<", cLT)
--         , (">", cGT)
--         , ("==", cEQ)
             ,("+", dOp2C "+" (+))
--          , ("-", dOp2C "-" (-))
           , ("*", dOp2C "*" (*))
          ,  ("0", num2C 0)
          ,  ("1", num2C 1)
--            , ("2", num2C 2)
--             , ("3", num2C 3)
--             , ("4", num2C 4)
--            ,  ("5", num2C 0)
           ]

intsto10 = CM.fromList $ [(show i, num2C i) | i <- [2..10]]

lib' = lib  `CM.union` routers -- `CM.union` intsto10

expGrammar = Grammar l c
    where l = CM.fromList $ [(c, (-3)) | c <- CM.elems lib']
          c = 0 -- (-3)

lib1 = lib `CM.union` one_routers
lib2 = lib `CM.union` two_routers
lib3 = lib `CM.union` three_routers
lib123 = lib `CM.union` routers

grammarFromLib lib = normalizeGrammar $ Grammar l 0
    where l = CM.fromList $ [(c, 0) | c <- CM.elems lib]
                     
mkSymRegExp name lib frontierSize 
    = Experiment {expName = name,
                  expTaskSet = symRegTaskSet',
                  expEps = 0,
                  expPrior = grammarFromLib lib,
                  expInitLib = grammarFromLib lib,
                  expDepthBound = 3,
                  expNumBound = frontierSize,
                  expReps = 25}

libSet = [lib1] -- lib3, lib123]
libNames = ["R1"] -- , "R3", "R<=3"]
frontierSizeSet = [9000, 10000]

expSet = concat [[mkSymRegExp (n ++ "_" ++ show f) l f | n <- libNames | l <- libSet]
              | f <- frontierSizeSet]

runExpSet = sequence $ (map runExp expSet) ++ (map runBruteForceExp expSet)


expSymReg 
    = Experiment {
        expName = "Symbolic Regression", 
        expTaskSet = taskSet',
        expEps = 0,
        expPrior = expGrammar,
        expInitLib = expGrammar,
        expDepthBound = 3,
        expNumBound = 10,
        expReps=10
      }

main = do
  runExpSet










  


 
