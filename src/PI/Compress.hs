-- Compress.hs
-- Eyal Dechter

-- | This module contains functions for compressing a set of
-- combinators.

module Compress where

import Control.Monad.State 
import Data.List (foldl')
import Debug.Trace

import CL
import qualified CombMap as CM
import CombMap (CombMap)


-- | Record all unique combinators that satisfy the following:

-- | When incrementing a tree:
-- a ) if it has count 0 set count to 1 and increment children
-- b) if it has count 1 set count to 2 and decrement children
-- c) if it has count > 2 set count (+1)
-- | When decrementing a tree:
-- a) if it has count 1 set count to 0 and decrement children
-- b) if it has count 2 or greater, set count (-1) and increment children


data Turn = L | R deriving (Show, Eq)
type Path = [Turn] 

data Pointer = Pointer Comb Path deriving (Show, Eq)

pointRight (Pointer c p) = Pointer c (p ++ [R])
pointLeft  (Pointer c p) = Pointer c (p ++ [L])


incr :: Pointer -- ^ a pointer to the current location in the comb tree
     -> Comb -- ^ the combinator we are current sitting at
     -> CombMap [Pointer] -- ^ the current index of pointers
     -> CombMap [Pointer] -- ^ the updated index of pointers
incr ptr@(Pointer comb path) c@(CApp l r _ _) ind = 
    case CM.lookup c ind of
      Nothing -> let ind' = CM.insert c [ptr] ind
                 in incr (pointRight ptr) r $ incr (pointLeft ptr) l ind'
      Just [] ->  let ind' = CM.insert c [ptr] ind
                  in incr (pointRight ptr) r $ incr (pointLeft ptr) l ind'
      Just ptrs@(ptr':[])  
          -> let ind' = CM.insert c (ptr:ptrs) ind
                 ind'' = decrChildren ptr' ind'
             in ind''
      Just ptrs -> CM.insert c (ptr:ptrs) ind
incr ptr c@(CNode{}) ind = case CM.lookup c ind of
                             Nothing -> CM.insert c [ptr] ind
                             Just xs -> CM.insert c (ptr:xs) ind

decr ptr@(Pointer comb path) c@(CApp l r _ _) ind = 
    case CM.lookup c ind of 
      Nothing -> error "Trying to decr a comb that isn't there"
      Just [] -> error "Trying to decr a comb that isn't there"
      Just (ptr':[]) -> let ind' = CM.insert c [] ind
                        in decrChildren ptr ind'
      Just (ptr':xs) -> let ind' = CM.insert c xs ind
                        in incrChildren ptr ind'
decr ptr c@(CNode{}) ind = 
      

decrChildren :: Pointer -> CombMap [Pointer] -> CombMap [Pointer]
decrChildren ptr@(Pointer (CApp l r _ _ ) path) ind
    = decr (pointRight ptr) r $ decr (pointLeft ptr) l ind

incrChildren :: Pointer -> CombMap [Pointer] -> CombMap [Pointer]
incrChildren ptr@(Pointer (CApp l r _ _ ) path) ind
    = incr (pointRight ptr) r $ incr (pointLeft ptr) l ind


      

-- incr index c@(CApp l r _ _) 
--     = case CM.lookup c index of
--         Nothing -> let index' = CM.insert c 1 index
--                    in incr (incr index' l) r
--         Just i -> let index' = CM.insert c (i + 1) index
--                   in case i  of
--                        0 -> incr (incr index' l) r
--                        1 -> decr (decr index' l) r 
--                        otherwise -> index'
-- incr index c@(CNode _ _ _) = case CM.lookup c index of
--                                Nothing -> CM.insert c 1 index
--                                Just i -> CM.insert c (i+1) index
                 

-- decr :: Index -> Comb -> Index
-- decr index c@(CApp l r _ _ ) 
--     = case CM.lookup c index of
--         Nothing -> error $  "Cannot find comb " ++ show c
--         Just i -> let index' = CM.insert c (i - 1) index
--                   in case i of 
--                        1 -> decr ( decr index' l) r
--                        otherwise -> incr (incr index' l) r
-- decr index c@(CNode _ _ _) = case CM.lookup c index of
--                                Nothing -> error $  "Cannot find comb " ++ show c
--                                Just i -> CM.insert c (i-1) index

-- compress :: [Comb] -> Index
-- compress cs = foldl' incr CM.empty cs


-------------------------------
-- | Alternative compression scheme 
-- 1) index and count all the unique
-- subexpressions in the library 
-- 2) the complexity of a set of
-- expressions is the number of unique subexpression.

getUniqueTrees :: Comb -> CombMap Int
getUniqueTrees c@(CNode _ _ _ ) = CM.singleton c 1 
                  
getUniqueTrees c@(CApp l r _ _ ) = let a = CM.singleton c 1 
                                       b = getUniqueTrees  l
                                       d = getUniqueTrees  r
                                   in d  `with` a `with` b 
    where with = CM.unionWith (+)



                      





                    

                    



