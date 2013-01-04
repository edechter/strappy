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

import qualified CombMap as CM
import CombMap (CombMap)
import qualified Compress as CP (getUniqueTrees, incr)
import Compress (Index, showIndex)
import Experiment

data SearchLogEntry = SearchLogEntry { searchIter :: Int,
                                       searchLib :: CombMap Int,
                                       searchSpace :: [Comb],
                                       searchNumHit :: Int,
                                       searchExplanation :: [(Task, Comb)],
                                       searchCompression :: CombMap Int
                                     } deriving Show
mkEmptySearchLog :: SearchLogEntry
mkEmptySearchLog  = SearchLogEntry 0 CM.empty [] 0 [] CM.empty

type Search a = State SearchLogEntry a 

runSearch :: Search a -> (a, SearchLogEntry)
runSearch s = runState s mkEmptySearchLog 

-- | Sort data by number of combinators matching each
sortData :: [(Task, [Comb])] -> [(Task, [Comb])]
sortData = sortBy (compare  `on` (length . snd))

              

gen' (x:xs) cs = join [gen' xs (i:cs) | i <- [0..]]
gen' [] x = [x]

-- | Generate list of solutions
gen :: [(Task, [Comb])] -> (Index, [(Task, Comb)]) -> [(Index, [(Task, Comb)])] 
gen ((d, cs):rest) (index, asc) =  concat vs
    where vs = [gen rest (index `with` CP.getUniqueTrees c, (d, c):asc) | c <- cs]
          with = CM.unionWith (+)
gen [] x = [x]

-- | Select solution with best score
select :: [(Index, [(Task, Comb)])] 
       -> (Index, [(Task, Comb)])
select xs = a
          where a = argmax  ( (* (-1)) . length . CM.keys . fst ) xs

-- | Depth first search
dfs :: [(Task, [Comb])] -> [(Task, Comb)]
dfs xs = zip (map fst xs) cs
    where xs' = sortData xs          
          cs = nwiseDependencySearch 2 (map snd xs)

-- | Depth first search with bounded number of solutions
dfsN :: [(Task, [Comb])] 
    -> Int -- ^ max number of solutions
    -> (Index, [(Task, Comb)])
dfsN xs n = select $ take n (gen xs' (CM.empty, []))
    where xs' = sortData xs


-- | Greedy 
greedy :: Index -> [(Task, [Comb])] -> (Index, [(Task, Comb)])
greedy lib xs = foldl' g (lib, []) xs
    where g (index, asc) (d, cs) = (index', (d, c'):asc)
              where with = CM.unionWith (+)
                    vs = [(index `with` CP.getUniqueTrees c, c) | c <- cs]
                    (index', c') = argmax ( (* (-1)) . length . CM.keys . fst ) vs

-- | GreedyN 
greedyN :: Int -> Index -> [(Task, [Comb])] -> (Index, [(Task, Comb)])
greedyN n lib xs = let xss = take n (List.permutations xs)
                       out = map (greedy lib) xss
                       best = argmax ((* (-1)) . length . CM.keys . fst) out
                   in best

-- | Get new library
newLibrary :: [Comb] -> Index
newLibrary cs = CM.fromList $  map g $ filter (\(_, i) -> i > 1) xs
    where ind = foldl' CP.incr CM.empty cs
          xs = CM.assocs ind
          g (c@(CApp{}) , i) = (c, i)
          g x = x

-- | Adjust with prior
adjust :: Index -> Index -> Index
adjust = CM.unionWith (+)
                                
                             
-- | For each data point in a dataset list all the expressions that
-- evaluate to that datapoint. 
findCombinatorsForEachDatum :: Experiment 
                            -> CombMap Int -- ^ current lib
                            -> Search [(Task, [Comb])]
findCombinatorsForEachDatum ex lib 
    = do s <- get
         return [(t, [ c | c <- db Map.! tp, 
                                  f c <= eps ]) | t@(Task n f tp) <- taskSet]
      where 
            cs = \t -> map fst $ runStateT (enumIB lib maxDepth 2000 t) 0
            db = foldl' (\m (k, a) -> Map.insert k a m) Map.empty 
                 [(t, cs t) | t <- ts]
            ts = nub [t | (Task _ _ t) <- taskSet]

            -- vars
            taskSet = expTaskSet ex
            eps = expEps ex
            maxDepth = expDepthBound ex
            
oneStep :: Experiment 
        -> Index -- ^ current lib
        -> Search Index
oneStep ex lib = do xs <- findCombinatorsForEachDatum ex lib
                    let xs' = filter (not . null . snd) xs
                        rs = (trace $  "Hit: " ++ show (length xs'))
                                       $ dfs  (sortData xs') 
                        index' =  (trace $ showTaskCombAssignments rs) 
                                  $ newLibrary $ map snd rs
                        index'' = adjust index' prior
                    return ((trace $ showIndex index'')  index'')
          -- vars
    where
      taskSet = expTaskSet ex
      eps = expEps ex
      maxDepth = expDepthBound ex
      tp = expDataType ex
      prior = expPrior ex

                           

loop :: Experiment -> Search Index
loop ex
    = foldM (\l _ -> oneStep ex l) lib [0..reps]
      where           
        -- vars
        eps = expEps ex
        maxDepth = expDepthBound ex
        tp = expDataType ex
        prior = expPrior ex
        lib = expInitLib ex
        reps = expReps ex







