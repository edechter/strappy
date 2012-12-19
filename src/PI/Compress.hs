-- Compress.hs
-- Eyal Dechter

-- | This module contains functions for compressing a set of
-- combinators.

module Compress where

import Control.Monad.State 
import Debug.Trace

import CL
import qualified CombTrie as CT


-- | Record all unique combinators that satisfy the following pro

type Index = CT.CombTrie Int

-- | When incrementing a tree:
-- a ) if it has count 0 set count to 1 and increment children
-- b) if it has count 1 set count to 2 and decrement children
-- c) if it has count > 2 set count (+1)
-- | When decrementing a tree:
-- a) if it has count 1 set count to 0 and decrement children
-- b) if it has count 2 or greater, set count (-1) and increment children


incr :: Index -> Comb -> Index
incr index c@(CApp l r _ _) 
    = case CT.lookup index c of
        Nothing -> let index' = CT.insert c 1 index
                   in incr (incr index' l) r
        Just i -> let index' = CT.insert c (i + 1) index
                  in case i  of
                       0 -> incr (incr index' l) r
                       1 -> decr (decr index' l) r 
                       otherwise -> index'
incr index c@(CNode _ _ _) = case CT.lookup index c of
                               Nothing -> CT.insert c 1 index
                               Just i -> CT.insert c (i+1) index
                  

decr :: Index -> Comb -> Index
decr index c@(CApp l r _ _ ) 
    = case CT.lookup index c of
        Nothing -> error $  "Cannot find comb " ++ show c
        Just i -> let index' = CT.insert c (i - 1) index
                  in case i of 
                       1 -> decr ( decr index' l) r
                       otherwise -> incr (incr index' l) r
decr index c@(CNode _ _ _) = case CT.lookup index c of
                               Nothing -> error $  "Cannot find comb " ++ show c
                               Just i -> CT.insert c (i-1) index

compress1  c = incr CT.empty c

compress :: [Comb] -> Index
compress (c:cs) = incr (compress cs) c
compress [] = CT.empty


-------------------------------
-- | Alternative compression scheme 
-- 1) index and count all the unique
-- subexpressions in the library 
-- 2) the complexity of a set of
-- expressions is the number of unique subexpression.

getUniqueTrees :: Comb -> Index
getUniqueTrees c@(CNode _ _ _ ) = CT.single c 1 
                  
getUniqueTrees c@(CApp l r _ _ ) = let a = CT.single c 1 
                                       b = getUniqueTrees  l
                                       d = getUniqueTrees  r
                                   in d  `with` a `with` b 
    where with = CT.mergeWith (+)



                      





                    

                    



