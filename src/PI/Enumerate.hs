-- Enumerate.hs
{-# Language ParallelListComp , BangPatterns #-}

module Enumerate where
    
import qualified Data.Map as Map 
import Data.List (sortBy)
import Control.Monad
import Control.Monad.State
import Control.Monad.List
import Control.Monad.Error
import Debug.Trace
import Text.CSV
import Data.Maybe

import Type
import CL
import StdLib
import Expr
import CLError
import CombMap (CombMap, (!))
import qualified CombMap as CM

enum :: [Comb] -> Int -> Type -> StateT Int [] Comb
enum xs d t = do
  t' <- freshInst t
  enum' xs d t'

enum' :: [Comb] -> Int -> Type -> StateT Int [] Comb
enum' xs 0 t = filterCombinatorsByType xs t

enum' xs d t = enum' xs 0 t `mplus` do
  tp <- newTVar Star
  let t_left0 = tp ->- t
  left <- enum' xs (d-1) t_left0
  let t_left1 = cType left
      t_right0 = fromType t_left1
  right <- enum' xs (d-1) t_right0
  let t_right1 = cType right
  tp' <- newTVar Star
  let backsub = fromJust $ mgu t_left1 (t_right1 ->- tp')
      combined 
          = CApp left right 
            (toType (apply backsub t_left1))
            (mkAppDepth left right)
  return combined


-- | Limited Breadth Enum
enumLB :: CombMap Int -> Int -> Int ->  Type -> StateT Int [] Comb
enumLB xs d b t = do
  t' <- freshInst t
  enumLB' xs d b t'

liftST :: ([(a, Int)] -> [(a,Int)]) -> (StateT Int [] a -> StateT Int [] a)
liftST f st = StateT $ \s -> 
              let xs = runStateT st s
              in f xs 

enumLB' :: CombMap Int -> Int -> Int -> Type -> StateT Int [] Comb
enumLB' xs 0 b t = let cs = filterCombinatorsByType (CM.keys xs) t 
                       g ys =take b $ 
                                 sortBy (\(a, _) (b, _) -> 
                                         ((-1) * xs ! a) `compare` ((-1) * xs ! b)) ys
                   in liftST g cs

                        
enumLB' xs d b t = enumLB' xs 0 b t `mplus` do
  tp <- newTVar Star
  let t_left0 = tp ->- t
  left <- enumLB' xs (d-1) b t_left0
  let t_left1 = cType left
      t_right0 = fromType t_left1
  right <- enumLB' xs (d-1) b t_right0
  let t_right1 = cType right
  tp' <- newTVar Star
  let backsub = fromJust $ mgu t_left1 (t_right1 ->- tp')
      combined 
          = CApp left right 
            (toType (apply backsub t_left1))
            (mkAppDepth left right)
  return combined

-- | Iterative Broadening Enum
enumIB :: CombMap Int
       -> Int -- max depth
       -> Int -- max num combinators
       -> Type
       -> StateT Int [] Comb
enumIB xs d n t = let m = CM.size xs
                  in liftST (take n) $ enumIB' (-1) 0
    where enumIB' last i = let st = enumLB xs d i t
                               num = length (runStateT st 0)
                           in if (trace $ show i) $ last < num && num < n 
                              then enumIB' num (i+1)
                              else st


-- type ST = StateT Int AmbTI
-- enumN :: [Comb] 
--       -> Int -- max depth
--       -> Int -- max num solutions
--       -> Type
--       -> AmbTI Comb
-- enumN xs d n t 
--     = do (a, s) <- runStateT ((lift . freshInst) t >>= enumN' xs d) n
--          return a

-- enumN' :: [Comb] -> Int -> Type -> ST Comb
-- enumN' xs 0 t 
--     = do m <- get
--          TypeInfT $ \s i -> 
--              Right $ (take m) $ runErrorT $ runTypeInfT (filterCombinatorsByType xs t) s i



