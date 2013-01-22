-- Search.hs
-- Eyal Dechter

-- | This module contains functions for searching over CL libraries
-- given a dataset.

module Search where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.List.Extras.Argmax (argmaxWithMaxBy, argmax)
import Data.List (sortBy, foldl', nub)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Maybe
import Data.Function (on)
import Debug.Trace

import Type
import CL
import Expr
import CLError
import EnumBF
import StdLib
import Task
import Data
import CompressionSearch
import ParseCL
import Grammar

import qualified CombMap as CM
import CombMap (CombMap)
import qualified Compress as CP (getUniqueTrees, compress2)
import Compress (Index)
import Experiment

data SearchLogEntry = SearchLogEntry { searchGrammar :: ! Grammar,
                                       searchExplanation :: ! [(Task, Comb)]
                                     } deriving Show


type Search a = Writer [SearchLogEntry] a 

runSearch :: Search a -> (a, [SearchLogEntry])
runSearch s = runWriter s 

mkHypothesisSpace :: Experiment 
                  -> Grammar 
                  -> Search (Map.Map Type [Comb])
mkHypothesisSpace expr gr
    = do return $ db
    where tps = nub [ taskType t | t <- taskSet]
          cs tp = map comb $ enumBF gr (expNumBound expr) tp
          db =  foldl' (\m (k, a) -> Map.insert k a m) 
                       Map.empty [(tp, cs tp) | tp <- tps]
          taskSet = expTaskSet expr



evalSymReg :: Comb -> [Int]
evalSymReg c = [ f i | i <- [0..10]]
    where f i = case reduceComb $ CApp c (num2C i) tInt 0 of 
                  (N y ) -> y
                  otherwise -> (maxBound :: Int)

evalDigArith :: Comb -> [Bool]
evalDigArith c = [ f i | i <- [0..10]]
    where f i = case reduceComb $ CApp c (num2C i) tInt 0 of 
                  (N y ) -> y
                  otherwise -> (maxBound :: Int)


-- | For each data point in a dataset list all the expressions that
-- evaluate to that datapoint. 
findCombinatorsForEachDatum :: Experiment 
                            -> Grammar -- ^ current grammar
                            -> Search [(Task, [Comb])]
findCombinatorsForEachDatum expr gr
    = do 
         db <- mkHypothesisSpace expr gr
         return $ do 
           t <- taskSet
           let cs = case t of 
                      Task n f tp -> do c <- db Map.! tp
                                        if f c <= eps then return c else mzero
                      SymRegTask n vs tp -> do c <- db Map.! tp
                                               if evalSymReg c == vs then return c else mzero
                      DigArithTask n vs tp -> do c <- db Map.! tp
                                                 if evalDigArith c == vs then return c else mzero
           return (t, cs) 
      where 
        taskSet = expTaskSet expr
        eps = expEps expr
        maxDepth = expDepthBound expr
            
oneStep :: Experiment 
        -> Grammar -- ^ current grammar
        -> Search Grammar
oneStep ex gr = do xs <- findCombinatorsForEachDatum ex gr
                   let xs' = filter (not . null . snd) xs
                       xsShort =  map (\(t, cs) -> (t, take 3 cs)) xs'  
                       rs = (trace $  "Hit: " ++ show (xsShort))
                            $ dfs xsShort
                       ind = CP.compress2 (map (\(tsk, c) -> (taskType tsk, c))  rs)
                       grammar' = (trace $ showTaskCombAssignments rs)
                                  $ estimateGrammar prior (0.1) ind rs  
                   tell $ [SearchLogEntry gr rs]
                   return $ (trace $ show grammar') $ grammar'

          -- vars
    where
      taskSet = expTaskSet ex
      eps = expEps ex
      maxDepth = expDepthBound ex
      prior = expPrior ex

loop :: Experiment -> Search Grammar
loop ex
    = foldM (\l _ -> oneStep ex l) grammar [0..(reps-1)]
      where           
        -- vars
        eps = expEps ex
        maxDepth = expDepthBound ex
        prior = expPrior ex
        grammar = expInitLib ex
        reps = expReps ex



bruteForceSearch :: Experiment 
                 -> Search Grammar
bruteForceSearch ex 
    = let n = (expNumBound ex) * (expReps ex)
          name = (expName ex) ++ "_Brute"
          exBrute = ex{expName=name,
                       expNumBound = n,
                       expReps = 1}
          gr = expInitLib ex
      in do xs <- findCombinatorsForEachDatum ex gr
            let xs' = filter (not . null . snd) xs
                rs' = map (\(t, (c:cs)) -> (t, c)) xs'
            tell $ [SearchLogEntry gr rs']
            return gr
                    
                                               
                                                 



