-- DigArith.hs
{-# Language ParallelListComp #-} 

module Strappy.DigArith where

import Control.Monad.State
import Data.Maybe
import qualified Data.HashMap as HMap
import Control.Monad.Random
import Debug.Trace

import Strappy.Experiment
import Strappy.Type

import Strappy.Type
import Strappy.CL
import Strappy.Expr
import Strappy.Task
import Strappy.StdLib
import Strappy.Data
import qualified Strappy.CombMap as CM
import Strappy.CombMap (CombMap)
import Strappy.Grammar
import Strappy.Routers 
import Strappy.EnumBF
import Strappy.ParseCL hiding (eval)



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
mkBoolTask name tt = DigArithTask name vs card tp
    where tp = mkTruthTableType tt
          card = truthTableCardinality tt
          vs = [out | (inp, out) <- tt]
          

mkAllBoolTasks :: Int -> [Task]
mkAllBoolTasks n = [mkBoolTask (name i) tt | tt <- mkAllTruthTables n | i <- [0..]]
                   where name i = "task_boolean" ++ show n ++ "_" ++ show i

evalDigArith :: Int -- ^ cardinality
             -> Comb 
             -> [Bool]
evalDigArith n c = [ toBool $ f vs | vs <- mkAllAss n]
    where
      f vs = reduceComb $ foldl (\x v -> app' x v) c $ map bool2C vs
      toBool c = case c of 
                   (B True) -> True
                   (B False) -> False

taskTruthTable :: Task -> TruthTable
taskTruthTable tsk = [(tup, v) | v <- vals | tup <- tups]
    where card = taskCard tsk
          vals = taskBoolVals tsk
          tups = mkAllAss card

showTruthTable :: TruthTable -> String
showTruthTable tt = unlines $ map showRow tt
    where showRow (tup, v) = (unwords $ map show tup) ++ 
                             " --> " ++ show v

