-- Compress.hs
-- Eyal Dechter

-- | This module contains functions for compressing a set of
-- combinators.

module Compress where

import Control.Monad.State 
import Debug.Trace

import CL
import qualified CombMap as CM
import CombMap (CombMap)


-- | Record all unique combinators that satisfy the following pro

type Index = CombMap Int

-- | When incrementing a tree:
-- a ) if it has count 0 set count to 1 and increment children
-- b) if it has count 1 set count to 2 and decrement children
-- c) if it has count > 2 set count (+1)
-- | When decrementing a tree:
-- a) if it has count 1 set count to 0 and decrement children
-- b) if it has count 2 or greater, set count (-1) and increment children

showIndex :: CombMap Int -> String
showIndex ct = unlines $ map (\(c, i) -> show i ++ ": " ++ 
                               show' c) (CM.toList ct)


incr :: Index -> Comb -> Index
incr index c@(CApp l r _ _) 
    = case CM.lookup c index of
        Nothing -> let index' = CM.insert c 1 index
                   in incr (incr index' l) r
        Just i -> let index' = CM.insert c (i + 1) index
                  in case i  of
                       0 -> incr (incr index' l) r
                       1 -> decr (decr index' l) r 
                       otherwise -> index'
incr index c@(CNode _ _ _) = case CM.lookup c index of
                               Nothing -> CM.insert c 1 index
                               Just i -> CM.insert c (i+1) index
                  

decr :: Index -> Comb -> Index
decr index c@(CApp l r _ _ ) 
    = case CM.lookup c index of
        Nothing -> error $  "Cannot find comb " ++ show c
        Just i -> let index' = CM.insert c (i - 1) index
                  in case i of 
                       1 -> decr ( decr index' l) r
                       otherwise -> incr (incr index' l) r
decr index c@(CNode _ _ _) = case CM.lookup c index of
                               Nothing -> error $  "Cannot find comb " ++ show c
                               Just i -> CM.insert c (i-1) index

compress1  c = incr CM.empty c

compress :: [Comb] -> Index
compress (c:cs) = incr (compress cs) c
compress [] = CM.empty


-------------------------------
-- | Alternative compression scheme 
-- 1) index and count all the unique
-- subexpressions in the library 
-- 2) the complexity of a set of
-- expressions is the number of unique subexpression.

getUniqueTrees :: Comb -> Index
getUniqueTrees c@(CNode _ _ _ ) = CM.singleton c 1 
                  
getUniqueTrees c@(CApp l r _ _ ) = let a = CM.singleton c 1 
                                       b = getUniqueTrees  l
                                       d = getUniqueTrees  r
                                   in d  `with` a `with` b 
    where with = CM.unionWith (+)



                      





                    

                    



