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
import Strappy.Expr
import Strappy.Task
import Strappy.Data
import Strappy.Library


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
mkBoolTask name tt = Task name vs tp
    where tp = mkTruthTableType tt
          card = truthTableCardinality tt
          vs = [out | (inp, out) <- tt]
          

mkAllBoolTasks :: Int -> [Task]
mkAllBoolTasks n = [mkBoolTask (name i) tt | tt <- mkAllTruthTables n | i <- [0..]]
                   where name i = "task_boolean" ++ show n ++ "_" ++ show i

evalDigArith :: Int -- ^ cardinality
             -> Expr
             -> [Bool]
evalDigArith n c = [ f vs | vs <- mkAllAss n]
    where
      f vs = eval $ foldl (\x v -> x <> v) c $ map bool2C vs

taskTruthTable :: Task -> TruthTable
taskTruthTable tsk = [(tup, v) | v <- vals | tup <- tups]
    where card = taskCard tsk
          vals = taskBoolVals tsk
          tups = mkAllAss card

showTruthTable :: TruthTable -> String
showTruthTable tt = unlines $ map showRow tt
    where showRow (tup, v) = (unwords $ map show tup) ++ 
                             " --> " ++ show v

