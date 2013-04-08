-- TestBranching.hs
{-# Language BangPatterns #-}

import Control.Monad 
import Control.Monad.State 

import Type
import CL
import StdLib
import CLError
import qualified Data.Map as Map

import Debug.Trace

  

ti1 = newTVar Star >> newTVar Star
ti2 = newTVar Star

ti' :: TI [ TI Type ]
ti' = newTVar Star >> newTVar Star >> return [ti1, ti2]

ti'' :: TI [ TI Type ]
ti'' = newTVar Star >> (return $ branch (return [newTVar Star]))


-- | this seems to work
-- | run with : map runTI $branch $ testfunc 3
testfunc :: Int -> TI [TI Type]
testfunc 0 = return [newTVar Star]
testfunc d = do xs <- testfunc 0 -- ^ list of TIs w single types
                ys <- f -- ^ list of TIs w paired types
                return $ xs ++ ys
             where f :: TI [TI Type]
                   f = do 
                     tia <- do t <- newTVar Star
                               return $  testfunc (d-1)
                     tib <- do t <- tia
                               s <- getSubst
                               return $ testfunc (d-1)
                     tic <- do  a <- tia
                                b <- tib
                                let c = do
                                      x <- a
                                      y <- b
                                      return $ do 
                                          tx <- x 
                                          ty <- y
                                          return $ tx ->- ty
                                return c
                     return tic

type TIState a = StateT Type TI a 


-- enum :: [Comb] -> Int -> [TIState Comb]
-- enum xs 0 
--     = do
--   c <- xs
--   let s = do t <- get
--              c' <- lift $ freshInstComb c
--              tc <- lift $ typeCheck c'
--              succ <- lift $ unify tc t
--              return c'
--   return s

-- enum xs d = do
--   left <- return $ do 
--               tp <- get
--               t  <- return $ newTVar Star
--               put (t ->- tp)
--   right <- 

branch :: TI [ TI a] -> [TI a]
branch m@(TI c) = runTI $ do xs <- m
                             return $ map f xs
    where f n = m >> n
              
enum' :: [Comb] -> Int -> Type -> [TI Comb]
enum' (c:cc) 0 t 
    = let rest = filterCombinatorsByType cc t
          first = do c' <- freshInstComb c
                     tc <- typeCheck c'
                     succ <- unify' tc t
                     return c'
          in case runTISafe first of
               (Right _) -> (first : rest)
               otherwise -> rest

enum' lib@(c:cc) d t
    = enum' lib 0 t ++ rest
      where 
        rest :: [TI Comb]
        rest = do
          let ti = do t <- newTVar Star
                      s <- getSubst
                      n <- return
              s <- getSubst
              n <- return $ getVarInt
              (TI f) <- return $ newTVar Star
              let t' = case f s n of
                         Right (_, _, t') -> t'
                         otherwise -> fail 
              ti_a <- enum' lib (d-1) (t' ->- t)
              return []
              

-- f :: TI Type -> (Type -> [TI a]) -> [TI a]
                            
                     
