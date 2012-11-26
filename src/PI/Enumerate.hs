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

branchTI :: TI [TI a] -> [ TI a ]
branchTI m = let ms = runTI m
      in map (\n -> m >> n) ms

enumCombsToDepth :: [Comb] -> Int -> Type -> [TI Comb]
enumCombsToDepth lib 0 t = filterCombinatorsByType lib t

enumCombsToDepth lib d tp = enumCombsToDepth lib 0 tp ++ f
    where 
      ti = newTVar
      f = do 
        a <- branchTI $ do t <- ti
                           tp' <- freshInst tp
                           return $ join [enumCombsToDepth lib i (Map t tp') 
                                          | i <- [0..d-1]]
        b <- branchTI $ do cl <- a
                           tc <- typeCheck cl
                           let t = fromType tc
                           return $ join [enumCombsToDepth lib i t 
                                          | i <- [0..d-1]]
        let c = do 
              cl <- a
              cr <- b
              let c = CApp cl cr []
              newT <- typeCheck c
              return c
        guard( hasTIError c)
        return c
        

enumCombsToProb :: CT.CombTrie Int -- ^ library
                -> Double -- ^ log prob minimum
                -> Int -- ^ max depth
                -> Type 
                -> [TI (Comb, Double)]
enumCombsToProb lib ll 0 tp 
    = [do {x <- c; return (x, ll)} | c <- xs | ll <- lls]
      where xs = filterCombinatorsByType (CT.keys lib) tp
            counts = [ fromJust $ CT.lookup lib x | x <- map runTI xs]
            sumcounts = sum counts
            lls  = [ (log (fromIntegral $ fromJust $ CT.lookup lib x)) 
                     - log (fromIntegral sumcounts) | x <- map runTI xs]
      
enumCombsToProb lib ll d tp = enumCombsToProb lib ll 0 tp 
                              ++ f 
    where ti = newTVar
          f = do
            a <-  branchTI $ 
                         do t <- ti
                            tp' <- freshInst tp
                            return $ join [enumCombsToProb lib ll i (Map t tp')
                                               | i <- [d-1]]                         
            let all = snd $ runTI a
            guard $ all > ll
            b <-  branchTI $ 
                  do (cl, _) <- a
                     tc <- typeCheck cl
                     let t = fromType tc
                     return $ join [enumCombsToProb lib (ll - all) i t
                                    | i <- [d-1]]                         
            let bll = snd $ runTI b
            let c = do
                   (cl, _) <- a
                   (cr, _) <- b
                   let r = CApp cl cr []
                   newT <- typeCheck r
                   return (r, bll + all)
            guard(hasTIError c)
            return c

-- | Enumerate a given number of combinators via iterative deepening
-- on the loglikelihood cost of the combinators.  The algorithm uses
-- enumCombsToProb as a subroutine.  We incrementally increase the log
-- prob minimum according to some schedule until we have the required
-- number of combinators. The minimum difference between the costs of
-- two trees is the minumum difference in the costs of two terminals. 
enumNCombsViaIterativeDeepening :: CT.CombTrie Int -- ^ library
                                -> Double
                                -> Int -- ^ number of trees
                                -> Type 
                                -> [TI (Comb, Double)]
enumNCombsViaIterativeDeepening = undefined






                                     
