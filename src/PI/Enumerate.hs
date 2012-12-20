-- Enumerate.hs
{-# Language ParallelListComp , BangPatterns #-}

module Enumerate where
    
import qualified Data.Map as Map 
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
import qualified CombTrie as CT

enum :: [Comb] -> Int -> Type -> TypeInfT [] Comb
enum xs d t = do
  t' <- freshInst t
  enum' xs d t'

enum' :: [Comb] -> Int -> Type -> TypeInfT [] Comb
enum' xs 0 t = filterCombinatorsByType xs t

enum' xs d t = enum xs 0 t `mplus` do
  tp <- newTVar Star
  left <- enum xs (d-1) (tp ->- t) 
  leftType <- typeCheck left
  let t' = fromType leftType
  right <- enum xs (d-1) t' 
  let combined = CApp left right [] 0 
--  typeCheck combined -- | is this necessary?
  return combined


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



