-- PCFG.hs

module PCFG where

import Control.Monad 
import qualified Data.Map as Map
import Data.Map ((!))
import Debug.Trace

import CL
import Type
import StdLib
import CLError

-- | A simple multinomial grammar in which the probability of an
-- expression is simply a multinomial over the available combinator
-- counts.


type MultinomialLib = Map.Map Comb Int
combLogLikelihood :: MultinomialLib -- ^ A map storing current
                                      -- counts for each combinator
                  -> Comb -- ^ the combinator of interest
                  -> Type -- ^ the specific type that this combinator
                          -- was chosen for
                  -> Either CLError (Double, Type) -- ^ the log prob of
                                           -- choosing that comb
combLogLikelihood lib c tp | (not . null $ cName c) 
    = do let n = cName c
             ws = filterCombinatorsByType (Map.keys lib) tp
         outType <- case lookup c ws of
                      (Just t) -> Right t
                      Nothing -> Left $ Default 
                                 $ "Cannot find " ++ show c 
                                       ++ " in set of matching combinators."
         goodCombs <- case map fst $ ws  of
                        [] -> Left $ Default 
                              $  "Can't match type " 
                               ++ show tp ++ " against combinator library. "
                        xs -> Right xs
         targetCount <- case Map.lookup c lib of
                          (Just m) -> Right $ m
                          Nothing -> Left $ Default 
                                     $  "Can't find " ++ show n ++ " in combinator library. "
         otherCounts <- case sequence [ Map.lookup k lib | k <- goodCombs] of
                          (Just ns) -> Right ns
                          Nothing -> Left $ Default
                                     $ "Can't find some combinators in library."
         let logDenom = (log . fromIntegral $  sum otherCounts) 
             logNum = (log . fromIntegral $ targetCount)             
         return $ (logNum - logDenom, outType)

combLogLikelihood lib c@(CApp c1 c2 []) tp 
    = do let a = getNewTVar tp
             tpLeft = Map a tp
         (leftLogLike, tpLeft') <- combLogLikelihood lib c1 tpLeft
         (rightLogLike, tpRight') <- combLogLikelihood lib c2 (fromType tpLeft')
         outType <- typeCheckTypeApp tpLeft' tpRight'
         return $ (leftLogLike + rightLogLike, outType)

combLogLikelihood' lib c tp = case combLogLikelihood lib c tp of
                                (Right x) -> x


countTreeFrequencies :: [Comb] -> MultinomialLib
countTreeFrequencies combs = count (Map.fromList []) combs
    where 
          count m (c@(CNode _ _ _):cs) = count m' cs
              where m' = Map.insertWith (\nv ov -> ov+1) c 1  m
          count m (c@(CApp c1 c2 []):cs) = count m' (c1:c2:cs)
              where m' = Map.insertWith (\nv ov -> ov+1) c 1  m
          count m (c@(CApp c1 c2 name):cs) = count m' cs
              where m' = Map.insertWith (\nv ov -> ov+1) c 1  m
          count m [] = m

countLeafFrequencies :: [Comb] -> MultinomialLib
countLeafFrequencies combs = count (Map.fromList []) combs
    where 
          count m (c@(CNode _ _ _):cs) = count m' cs
              where m' = Map.insertWith (\nv ov -> ov+1) c 1  m
          count m (c@(CApp c1 c2 []):cs) = count m (c1:c2:cs)
          count m (c@(CApp c1 c2 name):cs) = count m' cs
              where m' = Map.insertWith (\nv ov -> ov+1) c 1  m
          count m [] = m

relabelCombSubtree :: [String] -> Comb -> Comb -> Comb
relabelCombSubtree names c@(CNode _ _ _) toRelabel = c
relabelCombSubtree names c@(CApp c1 c2 []) toRelabel
    = if c==toRelabel then (CApp c1 c2 nextName)
      else CApp (relabelCombSubtree names c1 toRelabel)
               (relabelCombSubtree  names c2 toRelabel) []
          where f (('_':ss):xs) = ((read ss :: Int):(f xs))
                f (x:xs) = f xs
                f [] = [0]
                nextName = "_" ++ show ( maximum (f names) + 1)
relabelCombSubtree names c _= c
                                    
relabelCombsSubtrees :: [String] -> [Comb] -> Comb -> [Comb]
relabelCombsSubtrees names combs c 
    = map (\v -> relabelCombSubtree names v c) combs


    
         
         

    
  

                              
                              
                                        
