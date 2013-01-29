-- EX5 
-- symbolic regression with progressively more impoverished task sets
{-# Language ParallelListComp #-}


module Experiments.EX5 where

import System.Environment (getArgs)
import Control.Monad.Random
import Control.Monad.State 
import qualified  Data.HashMap as HMap

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

taskSetNoConstants = [mkQuadSymRegTask i j k | i <- [1..9] , j <- [1..9], k <- [0..9]]
                     ++ [mkQuadSymRegTask i j k | i <- [1..9] , j <- [0], k <- [0..9]]
                     ++ [mkQuadSymRegTask i j k | i <- [0] , j <- [1..9], k <- [0..9]]

taskSetNoLinear = [mkQuadSymRegTask i j k | i <- [1..9] , j <- [0..9], k <- [0..9]]
                  ++ [mkQuadSymRegTask i j k | i <- [0] , j <- [1..9], k <- [0..9]]

taskSetOnlyQuadratic = [mkQuadSymRegTask i j k | i <- [1..9] , j <- [0..9], k <- [0..9]]
taskSetOnlyComplexQuadratic = [mkQuadSymRegTask i j k | i <- [1..9] , j <- [1..9], k <- [1..9]]
taskSetUpToQuadratic = [mkQuadSymRegTask i j k | i <- [0..9] , j <- [0..9], k <- [0..9]]



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

jitteredGrammarFromLib :: MonadRandom m => HMap.Map k Comb -> m Grammar
jitteredGrammarFromLib lib = do 
  vs <- getRandomRs (-1.0,1.0)
  let lib' = CM.fromList [(c, 2 + k) | c <- CM.elems lib | k <- vs]
  return $ normalizeGrammar $ Grammar lib' 0
                     
mkSymRegExp :: MonadRandom m => String 
             -> HMap.Map String Comb 
             -> Int 
             -> [Task]
             -> m Experiment
mkSymRegExp name lib frontierSize taskSet 
    = do 
  lib' <- jitteredGrammarFromLib lib
  return $ Experiment {expName = "EX5_symreg" ++ name,
                  expTaskSet = taskSet,
                  expEps = 0,
                  expPrior = lib',
                  expInitLib = lib',
                  expDepthBound = 3,
                  expNumBound = frontierSize,
                  expReps = 15}

main :: IO ()
main = do
  switch <- fmap (read . head) getArgs
  let (taskname, taskset) = case switch of 
                              1 -> ("NoConstants", taskSetNoConstants)
                              2 -> ("NoLinear", taskSetNoLinear)
                              3 -> ("OnlyQuadratic", taskSetOnlyQuadratic)
                              4 -> ("OnlyComplexQuadratic", taskSetOnlyComplexQuadratic)
                              5 -> ("UpToQuadratic", taskSetUpToQuadratic)
                              _ -> error $ "No task name " ++ taskname
  let frontierSize = 10000
      name = taskname  ++  "_frontier"  ++ show frontierSize
  exp <- mkSymRegExp name lib1 frontierSize taskset
  runExp exp "data/EX5"










  


 
