-- Grammar.hs

module Grammar where

import Data.List (intersect)
import Control.Monad.State
import Debug.Trace
import Data.Maybe

import CL 
import Type
import qualified CombMap as CM
import CombMap (CombMap)
import Compress
import Task



data Grammar = Grammar { library :: CombMap Double, -- ^ neg log prob
                         expansions :: Double -- ^ neg log prob
                       } deriving Eq

showLibrary :: Show a => CombMap a  -> String
showLibrary ct = unlines $ map (\(c, i) -> show' c ++ ": " ++ 
                               show i) (CM.toList ct)

instance Show Grammar where
    show (Grammar lib c) = "Grammar\n------------------------" 
                           ++ "\nExpansions: " ++ show c
                           ++ "\n\nLibrary: \n-------\n" 
                           ++ showLibrary lib

nullGrammar :: Grammar
nullGrammar = Grammar CM.empty 0

logsumexp = log . sum . (map exp)

normalizeGrammar :: Grammar -> Grammar 
normalizeGrammar (Grammar lib ex)
    = let logTotalMass = logsumexp $ ex:(CM.elems lib)
          lib' = CM.map (+ (-logTotalMass)) lib
          ex' = ex - logTotalMass
      in Grammar lib' ex'
          
sum' (a, b) (c, d) = (a + b, c + d)

-- countAllUses :: [Comb] -- ^ count the number of uses of these combs
--              -> [Comb] -- ^ in these combs
--              -> CombMap Int
-- countAllUses cs xs = foldl1 (CM.unionWith (+)) $ map (countUses cs) xs

-- countAllExpansions :: [Comb] -> [Comb] -> Int
-- countAllExpansions cs xs = sum $ map (countExpansions cs) xs

-- countAllAlts :: [Comb] -> [(Task, Comb)] -> CombMap Int
-- countAllAlts cs xs = foldl1 (CM.unionWith (+)) 
--                      $ map (\(t, c) 
--                                 -> countAlts cs c (taskType t)) xs

-- countUses :: [Comb] -- ^ the primitive combinators
--           -> Comb  -- ^ a particular combinator
--           -> CombMap Int
-- countUses cs c | elem c cs = CM.singleton c 1
-- countUses cs c@(CNode {}) = CM.singleton c 1
-- countUses cs c@(CApp cl cr _ _) = CM.unionWith (+) l r
--     where l = countUses cs cl
--           r = countUses cs cr

-- countExpansions :: [Comb] -> Comb -> Int
-- countExpansions cs c | elem c cs  = 0
-- countExpansions cs c@(CNode {}) = 0
-- countExpansions cs c@(CApp cl cr _ _) = 1 + l +  r
--     where l = countExpansions cs cl 
--           r = countExpansions cs cr

-- countAlts :: [Comb] -- ^ number of times any of these
--           -> Comb -- ^ could have been used in this
--           -> Type -- ^ where the requested type is this
--           -> (CombMap Int)
-- countAlts cs c tp  | elem c cs = foldl1 (CM.unionWith (+))
--                                         $ map fst $ runStateT ms 0
--                    where ms = do alt <- filterCombinatorsByType cs tp
--                                  return $ CM.singleton alt 1
-- countAlts cs c@(CNode{}) tp  = foldl1 (CM.unionWith (+))
--                                         $ map fst $ runStateT ms 0
--                    where ms = do alt <- filterCombinatorsByType cs tp
--                                  return $ CM.singleton alt 1
-- countAlts cs (CApp cl cr _ _) tp  = foldl1 (CM.unionWith (+)) 
--                                     $ map fst $ runStateT ms 0
--     where ms = do t <- newTVar Star
--                   let t_left0 = (t ->- tp)
--                       left_ind = countAlts cs cl t_left0
--                       t_right0 = fromType (cType cl)
--                       right_ind = countAlts cs cr t_right0
--                   return $ CM.unionWith (+) left_ind right_ind

countExpansions :: Comb -> Int
countExpansions (CNode{}) = 0
countExpansions (CApp l r _ _) = 1 + countExpansions l  + countExpansions r

countAlts :: [Comb] -> Type -> CombMap Int
countAlts cs tp = let ms = do tp' <- freshInst tp
                              alt <- filterCombinatorsByType cs tp'
                              return $ CM.singleton alt 1
                  in foldl (CM.unionWith (+)) CM.empty $ map fst $ runStateT ms 0

combineGrammars :: (Grammar, Int) -> (Grammar, Int) -> Grammar
-- | Combine two grammars weighted by the number of observations (or
-- pseudo-observations) each has.
combineGrammars (Grammar lib1 ex1, ob1) (Grammar lib2 ex2, ob2) = 
    normalizeGrammar $ Grammar lib ex
        where lib = CM.unionWith (\a b -> f a ob1 b ob2) lib1 lib2
              f lp1 n lp2 m = log $ ((exp lp1) * (fromIntegral n) 
                               + (exp lp1) * (fromIntegral m)) 
                              
              ex = f ex1 ob1 ex2 ob2

bernLogProb :: Int -> Int -> Double
bernLogProb hits obs | obs >= hits = logI hits - logI obs  where logI = log . fromIntegral
bernLogProb hist obs | otherwise =
                         error "bernLogProb: # obs must be greater than # of hits"

estimateGrammar :: 
    Grammar -- ^ prior
    -> Int -- ^ number of pseudo-observations by which to weight the prior 
    -> CombMap [Type] -- ^ primitive combinators and their occurance counts
    -> [(Task, Comb)]
    -> Grammar
estimateGrammar prior psObs ind xs = 
    let ind' = (trace $ CM.showCombMap ind) $ CM.filter ((> 1) . length) ind
        combs = CM.keys ind'
        uses = CM.map length ind'
        exs = foldl (\i c -> i + countExpansions c) 0 (combs)
        alts = foldl1 (CM.unionWith (+)) 
               $ map (countAlts combs) (concat . CM.elems $ ind')
        logprobs =  CM.mapWithKey f uses
            where f c v = bernLogProb v w where
                      w = case (CM.lookup c alts) of 
                            Nothing -> error $ "estimateGrammar: cannot find "
                                       ++ show c ++ " in alternative map " 
                                      ++ show alts
                            Just k -> k
        nPossibleExs = exs + sum (CM.elems uses)
        logProbEx = bernLogProb exs nPossibleExs
        empiricalGr = Grammar logprobs logProbEx
    in combineGrammars (normalizeGrammar prior, psObs) (normalizeGrammar empiricalGr, 1)

calcLogProb :: Grammar 
            -> Type
            -> Comb
            -> Double 
-- | Returns the log probability of using the given
-- combinator when prompted by the given type, as prescribed by the
-- grammar.
calcLogProb gr tp c 
    = let m = filterCombinatorsByType (CM.keys $ library gr) tp
          altCs = map fst $ runStateT m 0
          combLps = [exp $ (library gr) CM.! x | x <- altCs] 
          logProbAll = log $ exp (expansions gr) + sum combLps
          combLogProb = (library gr) CM.! c - logProbAll
      in combLogProb

exLogProb :: Grammar -> Type -> Double
exLogProb gr tp  
    = let m = filterCombinatorsByType (CM.keys $ library gr) tp
          altCs = map fst $ runStateT m 0
          combLps = [exp $ (library gr) CM.! x | x <- altCs] 
          logProbAll = log $ exp (expansions gr) + sum combLps
          out = if null altCs then log (0.5)  -- ^ this log (0.5) is a
                                              -- hack. it should be 0,
                                              -- right? since there
                                              -- are no alternatives,
                                              -- the probability is
                                              -- 1. But this causes
                                              -- infinite expansions
                                              -- in the best-first search. 
                else expansions gr  - logProbAll
      in  out




          
                                
                                       
                      
    

          
    
    