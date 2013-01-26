-- DigitalArithmetic
{-# Language ParallelListComp #-} 

module Experiments.DigitalArithmetic where

import Control.Monad.State
import Data.Maybe
import qualified Data.HashMap as HMap
import Control.Monad.Random
import Debug.Trace
-- import Data.GraphViz

import Experiment
import Type

import Type
import CL
-- import Search
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
import DigArith
-- import Visualize


taskIncr1 = Task "pair incr 1"
            f
            ((TAp tPair tBool) ->- (TAp tPair tBool))
            where f c = if all (==True) [ismatch c i j | (i, j) <- points] then 0 else 1
                  pairIn0 = intTupleToPair (0, 0)
                  pairOut0 = intTupleToPair (0, 1)
                  pairIn1 = intTupleToPair (0, 1)
                  pairOut1 = intTupleToPair (1, 0)
                  pairIn2 = intTupleToPair (1, 0)
                  pairOut2 = intTupleToPair (1, 1)
                  points = [(pairIn0, pairOut0),
                            (pairIn1, pairOut1),
                            (pairIn2, pairOut2)]
                  ismatch c cin cout = pairEq (app' c cin) cout

taskNot = Task "not" f (tBool ->- tBool)
    where f c = if a &&  b then  0 else 1
              where
                a = reduceComb (app' c cTrue) == reduceComb cFalse
                b = reduceComb (app' c cFalse) == reduceComb cTrue

taskAnd = Task "and" f (tBool ->- tBool ->- tBool)
    where f c = if a1 && a2 && a3 && a4  then  0 else 1
              where
                a1 = reduceComb (app' (app' c cTrue) cTrue) == reduceComb cTrue
                a2 = reduceComb (app' (app' c cTrue) cFalse) == reduceComb cFalse
                a3 = reduceComb (app' (app' c cFalse) cTrue) == reduceComb cFalse
                a4 = reduceComb (app' (app' c cFalse) cFalse) == reduceComb cFalse

taskOr = Task "or" f (tBool ->- tBool ->- tBool)
    where f c = if a1 && a2 && a3 && a4  then  0 else 1
              where
                a1 = reduceComb (app' (app' c cTrue) cTrue) == reduceComb cTrue
                a2 = reduceComb (app' (app' c cTrue) cFalse) == reduceComb cTrue
                a3 = reduceComb (app' (app' c cFalse) cTrue) == reduceComb cTrue
                a4 = reduceComb (app' (app' c cFalse) cFalse) == reduceComb cFalse

taskXor = Task "xor" f (tBool ->- tBool ->- tBool)
    where f c = if a1 && a2 && a3 && a4  then  0 else 1
              where
                a1 = reduceComb (app' (app' c cTrue) cTrue) == reduceComb cFalse
                a2 = reduceComb (app' (app' c cTrue) cFalse) == reduceComb cTrue
                a3 = reduceComb (app' (app' c cFalse) cTrue) == reduceComb cTrue
                a4 = reduceComb (app' (app' c cFalse) cFalse) == reduceComb cFalse

taskFalse = Task "false" f tBool
    where f c = if reduceComb c == reduceComb cFalse then 0 else 1

taskTrue = Task "True" f tBool
    where f c = if reduceComb c == reduceComb cTrue then 0 else 1

        
lib = (CM.fromList $ 
         [
          ("I", cI)
         , ("True", cTrue)
--        , ("False", cFalse)
         ,("nand", cNand)
--         , ("&", cAnd)
--         , ("not", cNot)
--         , ("pair", cPair)
--         , ("fst", cFst)
--         , ("snd", cSnd)
--         , ("triple", cTriple)
--         , ("fst3", cFst3)
--         , ("snd3", cSnd3)
--         , ("thrd3", cThrd3)
           ]) 

lib1 = lib `CM.union` one_routers
lib2 = lib `CM.union` two_routers
lib12 = lib1 `CM.union` two_routers

grammarFromLib lib = normalizeGrammar $ Grammar l 0
    where l = CM.fromList $ [(c, 0) | c <- CM.elems lib]

expDigArith = Experiment {expName = "dig arith",
                          expTaskSet = mkAllBoolTasks 2,
--                           expTaskSet = [taskTrue, taskTrue,
--                                         taskFalse, taskFalse,
--                                         taskNot, taskNot, 
--                                         taskAnd, taskAnd, 
--                                         taskOr, taskOr, 
--                                         taskXor, taskXor],

                          expEps = 0,
                          expPrior = grammarFromLib lib12,
                          expInitLib = grammarFromLib lib12,
                          expDepthBound=3,
                          expNumBound = 2000,
                          expReps = 3}

jitteredGrammarFromLib :: HMap.Map k Comb -> Rand StdGen Grammar
jitteredGrammarFromLib lib = do 
  vs <- getRandomRs (-1.0,2.0)
  let lib' = CM.fromList [(c, 2 + k) | c <- CM.elems lib | k <- vs]
  return $ normalizeGrammar $ Grammar lib' 0

mkDigArithM :: String -> HMap.Map String Comb -> Int -> Rand StdGen Experiment
mkDigArithM name lib frontierSize 
    = do 
  lib' <- jitteredGrammarFromLib lib
  return $ Experiment {expName = name ++ "_wjittermany",
                       expTaskSet = mkAllBoolTasks 2 ++ mkAllBoolTasks 3,

                  expEps = 0,
                  expPrior = lib',
                  expInitLib = lib',
                  expDepthBound = 3,
                  expNumBound = frontierSize,
                  expReps = 15}



-- Visualization
fromRight (Right x) = x
fromRight (Left err) = error err

combstrs = ["((S nand) I)",
           "B",
            "(S nand)",
            "S",
            "nand",
            "C",
            "((S ((B (B nand)) nand)) ((S nand) I))",
            "(B ((S nand) I))",
            "(B (B (B ((S nand) I))))",
            "(B (B ((S nand) I)))",
            "I"]


cs = map (fromRight . parseExpr lib1) combstrs
combsgr = combsToGraph cs

-- visualize lib filename = 
--     do combstr <- fmap (filter (not . null)) $ getLibFromFile filename
       
--        let cs = take 1 $ map (fromRight . parseExpr lib ) combstr
--            gr = combsToGraph cs
--        runGraphviz (graphToDot params gr) Pdf "test.pdf"



main = do
  n <- newStdGen
  let (exp, a) = runRand (mkDigArithM "digarith_ec2_allbool2and3_overnight" lib1 2000) n
  runExp exp






