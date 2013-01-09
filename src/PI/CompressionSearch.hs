-- CompressionSearch.hs
{-# Language FlexibleInstances #-}

module CompressionSearch where

import Control.Monad.State
import Data.List (sortBy, foldl', nub)
import Data.List.Extras.Argmax (argmaxWithMaxBy, argmax)
import Data.Maybe
import qualified Data.HashMap as HM
import Data.Hashable
import Data.Function (on)
import Debug.Trace


import CL
import qualified CombMap as CM
import qualified Compress as CP (getUniqueTrees)

import CombMap (CombMap)
import Task
import GraphSearch


nullIndex :: CombMap Int
nullIndex = CM.empty

updateIndex :: CombMap Int -> Comb -> CombMap Int
updateIndex index c = CM.unionWith (+) index (CP.getUniqueTrees c)

mkIndex :: [Comb] -> CombMap Int
mkIndex = foldl updateIndex CM.empty  


fullDependencyEq node1 node2 = node1 == node2

pairwiseDependencyEq node1 node2 
    = (last .  assignment) node1 == (last . assignment) node2

pairwiseDependencyEdge m (x:[]) c = (n, CM.empty)
    where n = (length . CM.keys) (mkIndex [x])

pairwiseDependencyEdge m (x:x':xs) c = (n' - n, ind')
    where ind = mkIndex [x']
          ind' = updateIndex ind x
          f = (length . CM.keys)
          n = f ind
          n' = f ind'

pairwiseDependencySearch :: [[Comb]] -> [Comb]
pairwiseDependencySearch xs = (extension . fst) results
    where results = dfs' (length xs) (SearchNode 0 []) CM.empty w [] eq xs
          eq = pairwiseDependencyEq
          w = pairwiseDependencyEdge


nwiseDependencyEdge n m xs c | n > 1 = (k' - k, CM.empty)
    where (y:ys) = take n xs
          f = (length . CM.keys)
          k = f (mkIndex ys)
          k' = f $ mkIndex (y:ys)

nwiseDependencyEq n node1 node2
    = f node1 == f node2
      where f = (take n . reverse . assignment)
          
nwiseDependencySearch :: Int -> [[Comb]] -> [Comb]
nwiseDependencySearch n xs = (extension . fst) results
    where results = dfs' (length xs) (SearchNode 0 []) CM.empty w [] eq xs
          eq = nwiseDependencyEq n
          w = nwiseDependencyEdge n
          
-- | Sort data by number of combinators matching each
sortData :: [(Task, [Comb])] -> [(Task, [Comb])]
sortData = sortBy (compare  `on` (length . snd))

-- | Depth first search
dfs :: [(Task, [Comb])] -> [(Task, Comb)]
dfs xs =  zip (map fst xs) cs
    where xs' = sortData xs          
          cs = nwiseDependencySearch 2 (map snd xs)

-- | Greedy 
greedy :: CombMap Int -> [(Task, [Comb])] -> (CombMap Int, [(Task, Comb)])
greedy lib xs = foldl' g (lib, []) xs
    where g (index, asc) (d, cs) = (index', (d, c'):asc)
              where with = CM.unionWith (+)
                    vs = [(index `with` CP.getUniqueTrees c, c) | c <- cs]
                    (index', c') = argmax ( (* (-1)) . length . CM.keys . fst ) vs
          
          


