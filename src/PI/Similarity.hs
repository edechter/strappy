-- Similarity.hs

module Similarity where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import CL 
import StdLib (cI, cS)
import CLError
import qualified CombTrie as CT

-- | To get a similarity metric between two combinators we need to
-- compare the number of common subtrees between them. As a first
-- pass, we will just create a sorted list of subtrees and count the
-- overlap between any two trees. 

-- | Return a list of subcombinators in a combinator. Don't drill down
-- into named compound combinators.
subcombinators :: Comb -> [Comb]
subcombinators c@(CNode _ _ _) = [c]
subcombinators c@(CApp c1 c2 []) = c:(subcombinators c1 ++ subcombinators c2)
subcombinators c@(CApp c1 c2 _)   = [c]

-- | Return unique elements in list. 
nubOrd :: Ord a => [a] -> [a] 
nubOrd xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []                                      



-- | Unique subcombinators
uniqueSubcombinators :: [Comb] -> [Comb]
uniqueSubcombinators = nubOrd . (foldl1 (++)) . (map subcombinators)

additionalUniqueSubcombinators :: [Comb] -> Comb -> [Comb]
additionalUniqueSubcombinators cs c = 
    Set.toList $ Set.difference (Set.fromList $ uniqueSubcombinators [c]) (Set.fromList cs) 

-- | Count subcombinators
countSubcombinators :: Comb -> CT.CombTrie Int
countSubcombinators c@(CNode _ _ _) = CT.single c 1

countSubcombinators c@(CApp c1 c2 []) = CT.single c 1 `with`
                                        (countSubcombinators c1) `with`
                                        (countSubcombinators c2)
                                        where with = CT.mergeWith (+)
countSubcombinators c@(CApp c1 c2 name) = CT.single c 1



