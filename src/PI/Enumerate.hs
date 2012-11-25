-- Enumerate.hs
{-# Language ParallelListComp , BangPatterns #-}

module Enumerate where
    
import qualified Data.Map as Map 
import Control.Monad
import Debug.Trace
import Text.CSV
import Data.Maybe

import Type
import CL
import StdLib
import Expr
import CLError
import qualified CombTrie as CT

k :: TI [TI a] -> [ TI a ]
k m = let ms = runTI m
      in map (\n -> m >> n) ms
         

enumCombsToDepth :: [Comb] -> Int -> Type -> [TI Comb]
enumCombsToDepth lib 0 t = filterCombinatorsByType lib t

enumCombsToDepth lib d tp = enumCombsToDepth lib 0 tp ++ f
    where 
      ti = newTVar
      f = do 
        a <- k $ do t <- ti
                    tp' <- freshInst tp
                    return $ join [enumCombsToDepth lib i (Map t tp') 
                                  | i <- [0..d-1]]
        b <- k $ do t <- ti
                    return $ join [enumCombsToDepth lib i t 
                                   | i <- [0..d-1]]
        let c = do t <- ti
                   cl <- a
                   cr <- b
                   let c = CApp cl cr []
                   newT <- typeCheck c
                   return c
        guard( hasTIError c)
        return c
        

-- enumCombsToProb :: CT.CombTrie Int -- ^ library
--                 -> Double -- ^ log prob minimum
--                 -> Int -- ^ max depth
--                 -> Type 
--                 -> [TI (Comb, Double)]
-- enumCombsToProb lib ll 0 tp 
--     = [(c, ll) | (c, t) <- xs | ll <- lls]
--       where xs = filterCombinatorsByType (CT.keys lib) tp
--             counts = [ fromJust $ CT.lookup lib x | (x, _) <- xs]
--             sumcounts = sum counts
--             lls  = [ (log (fromIntegral $ fromJust $ CT.lookup lib x)) 
--                      - log (fromIntegral sumcounts) | (x, _) <- xs]
      
-- enumCombsToProb lib ll d tp = enumCombsToProb lib ll 0 tp 
--                               ++ f ll (d-1) tp
--     where f ll b tp = do
--             let tp' = Map (getNewTVar tp) tp
--             (a, atype, all) <-  enumCombsToProb lib ll b (normalizeType tp') 
--             guard $ all > ll
--             (b, btype, bll) <-  enumCombsToProb lib (ll - all) b (fromType atype) 
--             let outComb = CApp a b []
--                 outType =  case typeCheckTypeApp atype btype  of 
--                              (Right  t) -> t
--                              (Left err) -> error $ "Error in enumCombs: " 
--                                            ++ "\n" ++ showError err 
--             return $! (outComb, outType, bll + all)

-- | Enumerate a given number of combinators via iterative deepening
-- on the loglikelihood cost of the combinators.  The algorithm uses
-- enumCombsToProb as a subroutine.  We incrementally increase the log
-- prob minimum according to some schedule until we have the required
-- number of combinators. The minimum difference between the costs of
-- two trees is the minumum difference in the costs of two terminals. 
enumNCombsViaIterativeDeepening :: CT.CombTrie Int -- ^ library
                                -> Int -- ^ number of trees
                                -> Type 
                                -> [(Comb, Type)]
enumNCombsViaIterativeDeepening = undefined






                                     
