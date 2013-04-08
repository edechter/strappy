-- Compress.hs
-- Eyal Dechter

-- | This module contains functions for compressing a set of
-- combinators.

module Strappy.Compress where

import Control.Monad.State 
import Data.List (foldl')
import Debug.Trace

import Strappy.CL
import Strappy.Type
import qualified Strappy.CombMap as CM
import Strappy.CombMap (CombMap)


-- | Record all unique combinators that satisfy the following:

type Index = CombMap Int

-- | When incrementing a tree:
-- a ) if it has count 0 set count to 1 and increment children
-- b) if it has count 1 set count to 2 and decrement children
-- c) if it has count > 2 set count (+1)
-- | When decrementing a tree:
-- a) if it has count 1 set count to 0 and decrement children
-- b) if it has count 2 or greater, set count (-1) and increment children


incr :: Index -> Comb -> Index
incr index c@(CApp l r _ _) 
    = case CM.lookup c index of
        Nothing -> let index' = CM.insert c 1 index
                   in incr (incr index' l) r
        Just i -> let index' = CM.insert c (i + 1) index
                  in case i  of
                       0 -> incr (incr index' l) r
                       otherwise -> index'
incr index c@(CNode _ _ _) = case CM.lookup c index of
                               Nothing -> CM.insert c 1 index
                               Just i -> CM.insert c (i+1) index

                 
compress :: [Comb] -> Index
compress cs = foldl' incr CM.empty cs



incr2 :: CombMap [Type] -> Comb -> Type -> State Int (CombMap [Type])
incr2 index c@(CApp l r _ _) tp 
    = do t <- newTVar Star
         let t_left = t ->- tp
         case CM.lookup c index of
           Nothing -> do let index' = CM.insert c [tp] index 
                         l_index <- incr2 index' l t_left
                         r_index <- incr2 l_index r (fromType (cType l))
                         return r_index
           Just (x:[]) -> do let index' = CM.insert c [tp, x] index 
                             l_index <- incr2 index' l t_left
                             r_index <- incr2 l_index r (fromType (cType l))
                             return r_index
           Just xs -> do let index' = CM.insert c (tp:xs) index 
                         return index'
incr2 index c@(CNode _ _ _) tp = return $ CM.insertWith (++) c [tp] index 

compress2 :: [(Type, Comb)] -> CombMap [Type]
compress2 xs = foldl' f  CM.empty xs
    where f ind (t, c) = fst $ runState (incr2 ind c t) 0




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



                      





                    

                    



