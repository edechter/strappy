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
import qualified Compress as CP (getUniqueTrees, compress)
import Compress (Index)
import Experiment

data SearchLogEntry = SearchLogEntry { searchIter :: Int,
                                       searchLib :: Grammar,
                                       searchSpace :: [Comb],
                                       searchNumHit :: Int,
                                       searchExplanation :: [(Task, Comb)],
                                       searchCompression :: CombMap Int
                                     } deriving Show
mkEmptySearchLog :: SearchLogEntry
mkEmptySearchLog  = SearchLogEntry 0 nullGrammar [] 0 [] CM.empty

type Search a = State SearchLogEntry a 

runSearch :: Search a -> (a, SearchLogEntry)
runSearch s = runState s mkEmptySearchLog 

mkHypothesisSpace :: Experiment 
                  -> Grammar 
                  -> Search (Map.Map Type [Comb])
mkHypothesisSpace expr gr
    = do return db
    where tps = nub [ taskType t | t <- taskSet]
          cs tp = map comb $ enumBF gr (expNumBound expr) tp
          db = (trace $ "tps: " ++ show tps) $ foldl' (\m (k, a) -> Map.insert k a m) 
                       Map.empty [(tp, cs tp) | tp <- tps]
          taskSet = expTaskSet expr


-- | For each data point in a dataset list all the expressions that
-- evaluate to that datapoint. 
findCombinatorsForEachDatum :: Experiment 
                            -> Grammar -- ^ current grammar
                            -> Search [(Task, [Comb])]
findCombinatorsForEachDatum expr gr
    = do s <- get
         db <- mkHypothesisSpace expr gr
         return $ (trace $ show $ nub [taskType t | t <- taskSet]) $ [(t, [ c | c <- db Map.! tp, 
                       f c <= eps ]) | t@(Task n f tp) <- taskSet]
      where 
        taskSet = expTaskSet expr
        eps = expEps expr
        maxDepth = expDepthBound expr
            
oneStep :: Experiment 
        -> Grammar -- ^ current grammar
        -> Search Grammar
oneStep ex gr = do xs <- findCombinatorsForEachDatum ex gr
                   let xs' = filter (not . null . snd) xs
                       rs = (trace $  "Hit: " ++ show (length xs'))
                            $ dfs xs'
                       ind = CP.compress (map snd rs)
                       grammar' = (trace $ showTaskCombAssignments rs) 
                                  $ estimateGrammar prior 1 ind rs  

                   return $ combineGrammars (prior, 1) (grammar', 1)
          -- vars
    where
      taskSet = expTaskSet ex
      eps = expEps ex
      maxDepth = expDepthBound ex
      prior = expPrior ex

loop :: Experiment -> Search Grammar
loop ex
    = foldM (\l _ -> oneStep ex l) grammar [0..reps]
      where           
        -- vars
        eps = expEps ex
        maxDepth = expDepthBound ex
        prior = expPrior ex
        grammar = expInitLib ex
        reps = expReps ex







