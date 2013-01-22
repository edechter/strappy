-- DigitalArithmetic
{-# Language ParallelListComp #-} 

module Experiments.DigitalArithmetic where

import Control.Monad.State
import Data.Maybe
import qualified Data.HashMap as HMap
import Control.Monad.Random
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

type TruthTable = [([Bool], Bool)]
truthTableCardinality :: TruthTable -> Int
truthTableCardinality [] = 0
truthTableCardinality ((ass, v):xs) = 
    let card = length ass
    in case all ((==card) . length . fst) xs of
         True -> card
         False -> error $ "in truthTableCardinality: " 
                  ++ "all assignments are not of equal length."

mkAllAss 0 = []
mkAllAss 1 = [[True], [False]]
mkAllAss n = concat [[True:xs, False:xs] | xs <- mkAllAss $ n - 1]

mkAllTruthTables :: Int -- ^ cardinality
                 -> [TruthTable]
mkAllTruthTables n = [[(a, val) | val <- vals | a <- ass] | vals <- allvals]
    where ass = mkAllAss n
          allvals = mkAllAss (2^n)

mkTruthTableType tt = foldr1 (->-) $ replicate (card + 1) tBool
    where card = truthTableCardinality tt
    
mkBoolTask :: String -> TruthTable -> Task
mkBoolTask name tt = Task name f tp
    where tp = mkTruthTableType tt
          card = truthTableCardinality tt
          f c = (trace $  name) $ if all (==True) [ verify_assignment inp out | (inp, out) <- tt] then 0 else 1
              where verify_assignment vs o 
                        = reduceComb (cTest $ map bool2C vs) == reduceComb (bool2C o)
                    cTest vs =  foldl (\x v -> app' x v) c vs

mkAllBoolTasks :: Int -> [Task]
mkAllBoolTasks n = [mkBoolTask (name i) tt | tt <- mkAllTruthTables n | i <- [0..]]
                   where name i = "task_boolean" ++ show n ++ "_" ++ show i

evalDigArith :: Comb -> [Bool]
evalDigArith c = [ f i | i <- [0..10]]
    where f i = case reduceComb $ CApp c (num2C i) tInt 0 of 
                  (N y ) -> y
                  otherwise -> (maxBound :: Int)

        
lib = (CM.fromList $ 
         [
          ("I", cI)
 --        , ("True", cTrue)
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
                       expTaskSet = mkAllBoolTasks 3,
--                        expTaskSet = [taskNot, taskNot, 
--                                      taskAnd, taskAnd, 
-- --                                     taskTrue, taskTrue,
-- --                                     taskFalse, taskFalse,
--                                      taskOr, taskOr, 
--                                      taskXor, taskXor,
--                                                 taskIncr1],
                  expEps = 0,
                  expPrior = lib',
                  expInitLib = lib',
                  expDepthBound = 3,
                  expNumBound = frontierSize,
                  expReps = 5}


main = do
  n <- newStdGen
  let (exp, a) = runRand (mkDigArithM "digarith" lib1 100) n
  runExp exp






