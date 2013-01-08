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
import Enumerate
import StdLib
import Task
import Data
import CompressionSearch
import ParseCL
import Grammar

import qualified CombMap as CM
import CombMap (CombMap)
import qualified Compress as CP (getUniqueTrees, incr)
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


-- | For each data point in a dataset list all the expressions that
-- evaluate to that datapoint. 
findCombinatorsForEachDatum :: Experiment 
                            -> Grammar -- ^ current grammar
                            -> Search [(Task, [Comb])]
findCombinatorsForEachDatum ex grammar
    = do s <- get
         return [(t, [ c | c <- db Map.! tp, 
                                  f c <= eps ]) | t@(Task n f tp) <- taskSet]
      where 
            cs = \t -> map fst $ runStateT (enumIB (library grammar) maxDepth 2000 t) 0
            db = foldl' (\m (k, a) -> Map.insert k a m) Map.empty 
                 [(t, cs t) | t <- ts]
            ts = nub [t | (Task _ _ t) <- taskSet]

            -- vars
            taskSet = expTaskSet ex
            eps = expEps ex
            maxDepth = expDepthBound ex
            
oneStep :: Experiment 
        -> Grammar -- ^ current grammar
        -> Search Grammar
oneStep ex lib = do xs <- findCombinatorsForEachDatum ex lib
                    let xs' = filter (not . null . snd) xs
                        rs = (trace $  "Hit: " ++ show (length xs'))
                                       $ dfs  (sortData xs') 
                        grammar' =  (trace $ showTaskCombAssignments rs) 
                                  $ estimateGrammar $ map snd rs
                        grammar'' = addGrammars grammar' priorGrammar
                    return grammar''
          -- vars
    where
      taskSet = expTaskSet ex
      eps = expEps ex
      maxDepth = expDepthBound ex
      tp = expDataType ex
      priorGrammar = expPrior ex

                           

loop :: Experiment -> Search Grammar
loop ex
    = foldM (\l _ -> oneStep ex l) grammar [0..reps]
      where           
        -- vars
        eps = expEps ex
        maxDepth = expDepthBound ex
        tp = expDataType ex
        prior = expPrior ex
        grammar = expInitLib ex
        reps = expReps ex







