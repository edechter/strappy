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

-- estimateGrammar :: [Comb] -> Grammar
-- -- | Generate a new grammar from a set of combinators using the
-- -- combinator tree compression scheme defined in Compress.hs
-- -- (compress). Include all trees from the compression that occur more
-- -- than once (i.e. Sequitur / Neville-Manning algorithm). To estimate
-- -- the expansions, take the total number of trees in from the
-- -- compression and subtract the number of input combinators.
-- estimateGrammar cs = Grammar lib c
--     where ind = compress cs
--           xs = CM.assocs ind
--           count = CM.fold (+) 0 ind -- sum of tree counts in index
--           rootOverlap = length $ cs `intersect` (map fst xs)
--           lib = CM.fromList $ filter ((>1) . snd) xs 
--           c = (count - rootOverlap) `div` 2

-- addGrammars :: Grammar -> Grammar -> Grammar
-- -- | Combine 2 grammars, adding their counts where they overlap. 
-- addGrammars (Grammar l e) (Grammar l' e') = 
--     Grammar (CM.unionWith (+) l l') ( e + e')

countAlts :: [Comb] 
          -> CombMap Int
          -> Comb
          -> Type
          -> (CombMap Int, Int)
countAlts cs ind c tp = let out = map fst $ runStateT (countAlts' cs ind c tp) 0
                            inds = map fst out
                            exs = map snd out
                            indOut = foldl1 (CM.unionWith (+)) inds
                            exOut = sum exs
                        in (indOut, exOut)

countAlts' :: [Comb] -- ^ list of combinator primitives
          -> CombMap Int -- ^ alternative counts
          -> Comb -- ^ a chosen combinator
          -> Type -- ^ the requesting type
          -> StateT Int [] (CombMap Int, Int) -- ^ alternative counts, num expansions
countAlts' cs ind c tp 
    | elem c cs = do alt <- filterCombinatorsByType cs tp
                     return $ (CM.insertWith (+) alt 1 ind, 0)
    | otherwise = do t <- newTVar Star
                     let t_left0 = (t ->- tp)
                     (left_ind, left_ex) <- countAlts' cs ind (lComb c) t_left0
                     let t_right0 = toType (cType (lComb c))
                     (right_ind, right_ex) <- countAlts' cs ind (rComb c) t_right0
                     let newInd = CM.unionWith (+) left_ind right_ind
                         newEx  = 1 + right_ex + left_ex
                     return (newInd, newEx)


combineGrammars :: (Grammar, Int) -> (Grammar, Int) -> Grammar
-- | Combine two grammars weighted by the number of observations (or
-- pseudo-observations) each has.
combineGrammars (Grammar lib1 ex1, ob1) (Grammar lib2 ex2, ob2) = 
    Grammar lib ex
        where lib = CM.unionWith (\a b -> f a ob1 b ob2) lib1 lib2
              f lp1 n lp2 m = ((exp lp1) * (fromIntegral n) 
                               + (exp lp1) * (fromIntegral m)) 
                              / (fromIntegral $ n + m)
              ex = f ex1 ob1 ex2 ob2

bernLogProb :: Int -> Int -> Double
bernLogProb hits obs | obs >= hits = logI hits - logI obs  where logI = log . fromIntegral
bernLogProb hist obs | otherwise =
                         error "bernLogProb: # obs must be greater than # of hits"

estimateGrammar :: 
    Grammar -- ^ prior
    -> Int -- ^ number of pseudo-observations by which to weight the prior 
    -> CombMap Int -- ^ primitive combinators and their occurance counts
    -> [(Task, Comb)]
    -> Grammar
estimateGrammar prior psObs ind xs = 
    let combs = CM.keys ind
        (alts, exs) = unzip $ map (\(t, c) -> countAlts combs CM.empty c (taskType t)) xs
        altCounts = foldl1 (CM.unionWith (+)) alts
        nEx = sum exs
        logprobs = CM.mapWithKey f ind
            where f c v = bernLogProb (altCounts CM.! c) v
        nPossibleExs = nEx + sum (CM.elems ind)
        logProbEx = bernLogProb nEx nPossibleExs
        empiricalGr = Grammar logprobs logProbEx
      in combineGrammars (prior, psObs) (empiricalGr, 1)

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
          out = if null altCs then 0 else expansions gr  - logProbAll
      in  out




          
                                
                                       
                      
    

          
    
    