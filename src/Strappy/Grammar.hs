-- Grammar.hs

module Strappy.Grammar where

import Data.List (intersect, sortBy)
import Control.Monad.State
import Debug.Trace
import Data.Maybe

import Strappy.CL 
import Strappy.Type
import qualified Strappy.CombMap as CM
import Strappy.CombMap (CombMap)
import Strappy.Compress
import Strappy.Task

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

findCLeafInGrammar :: Grammar -> Comb -> Maybe Comb
-- | Is the combinator leaf in the grammar? If so, return it. Else Nothing. 
findCLeafInGrammar gr c@CLeaf{cName=n} = lookup n asc
    where cs = CM.keys (library gr)
          asc = [(cName c, c) | c <- cs, isCLeaf c]
findCLeafInGrammar gr c= error $ show c ++ " is not a CLeaf."

refreshCombFromGrammar :: Grammar -> Comb -> Maybe Comb
-- | Grab the combinator from the grammar. Useful, to get the original
-- type information.
refreshCombFromGrammar gr c@(CLeaf{cName=n}) = findCLeafInGrammar gr c
refreshCombFromGrammar gr c@CApp{lComb=cl, rComb=cr} = do cl' <- refreshCombFromGrammar gr cl
                                                          cr'<-  refreshCombFromGrammar gr cr
                                                          let c' = c{lComb=cl', rComb=cr'}
                                                          t <- case getType c' of 
                                                                 Right x -> Just x
                                                                 Left _ -> Nothing
                                                          return $ c'{cType=t}
refreshCombFromGrammar _ x = Just x

nullGrammar :: Grammar
nullGrammar = Grammar CM.empty 0

logsumexp xs = a + (log . sum . map (exp . (\x -> x-a)) $ xs)
    where a = maximum xs

normalizeGrammar :: Grammar -> Grammar 
normalizeGrammar (Grammar lib ex)
    = let logTotalMass = logsumexp $ ex : (CM.elems lib)
          lib' = CM.map (\x -> x - logTotalMass) lib
          ex' = ex - logTotalMass
      in Grammar lib' ex' 
          
countExpansions :: Comb -> Int
-- | Returns the number of expansions used in this combinator.
countExpansions c = count 0 c
    where count n (CLeaf{}) = n
          count n CApp{lComb=l, rComb=r} = let n' = count (n + 1) l
                                           in count n' r
countAlts :: [Comb]           -- ^ a list of combinators (e.g. from a library)
          -> Type             -- ^ a type t  
          -> CombMap Int      -- ^ a map of combinators that return type t
-- | 
countAlts cs tp = let ms = do tp' <- freshInst tp
                              alt <- filterCombinatorsByType cs tp'
                              return $ CM.singleton alt 1
                  in foldl (CM.unionWith (+)) CM.empty $ map fst $ runStateT ms 0

combineGrammars :: (Grammar, Double) -> (Grammar, Double) -> Grammar
-- | Combine two grammars weighted by the number of observations (or
-- pseudo-observations) each has.
combineGrammars (Grammar lib1 ex1, ob1) (Grammar lib2 ex2, ob2) = 
    normalizeGrammar $ Grammar lib ex
        where lib = CM.unionWith (\a b -> f a ob1 b ob2) lib1 lib2
              f lp1 n lp2 m = log $ ((exp lp1) * ( n) 
                               + (exp lp2) * ( m)) 
                              
              ex = f ex1 ob1 ex2 ob2

bernLogProb :: Int -> Int -> Double
bernLogProb hits obs | obs >= hits = logI hits - logI obs  where logI = log . fromIntegral
bernLogProb hits obs | otherwise =
                         error $ "bernLogProb: # obs " ++ show obs ++ 
                               " must be greater than # of hits " ++ show hits

showCombWType cs = unlines $ map show [(c, cType c) | c <- cs]

estimateGrammar :: 
    Grammar           -- ^ prior
    -> Double         -- ^ number of pseudo-observations by which to weight the prior 
    -> CombMap [Type] -- ^ primitive combinators and their occurance counts
    -> [(Task, Comb)]
    -> Grammar
estimateGrammar prior psObs ind xs = 
    let ind' = (trace $ CM.showCombMap ind) $ CM.filter ((> 1) . length) ind
        combs = map (fromJust . refreshCombFromGrammar prior) $ CM.keys ind'
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
        empiricalGr = Grammar logprobs 0 -- logProbEx
    in combineGrammars (normalizeGrammar prior, psObs) (normalizeGrammar empiricalGr, 1.0)


-- Estimates the production probabilities for the given grammar
estimateGrammarWeighted ::
  Grammar
  -> Double  -- pseudo-counts
  -> [(Comb, Double)]  -- Weighted observations
  -> Grammar
estimateGrammarWeighted gr@(Grammar{library=lib}) pseudoCounts obs =
  -- We want to compute, for each combinator, a fresh type
  -- First, find where the type variables start at within the observations,
  -- so that the fresh instances don't bind to these type variables
  -- typeLibrary will hold this
  let nextTVar = maximum $ map (maxTVarComb . fst) obs
      typeLibrary = map (freshInst' nextTVar) $ CM.keys lib
      -- Count up all uses and all expansions
      countAllUses = mapM_ (\(c,wt) -> countUses c wt typeLibrary) obs
      countAllExpansions = mapM_ (\(c,wt) -> countExpansions c wt) obs
      pseudoMap = CM.mapWithKey (\c _ -> (pseudoCounts,pseudoCounts)) lib
      -- Run the monadic actions w/ the correct prior pseudocounts
      lib' = CM.map (\(uses,attempts)-> negate $ log $ uses/attempts) $
             execState countAllUses pseudoMap 
      expansionsProb = negate $ log $ uncurry (/) $
                       execState countAllExpansions (0.0, 0.0)
  in
   Grammar{ library = lib', expansions = -log expansionsProb}
  where freshInst' nextTVar comb =
          (comb,
           fromJust $ evalStateT (freshInst $ cType comb) (nextTVar+1))
        countUses :: Comb -> Double -> [(Comb,Type)] -> State (CombMap (Double, Double)) ()
        -- Counts the number of times each combinator could have been used,
        -- and actually was used, in the given library
        countUses c wt tyLib | CM.member c lib = do
          let requested = cReqType c
          let alts = filter (\(_,ty) -> isJust $ mgu ty requested) tyLib
          -- Record that we attempted to use everything in alt
          forM_ alts $ \(otherComb,_) ->
            modify $ CM.adjust (\(uses,attempts)->(uses,attempts+wt)) otherComb
          -- Record that we actually used c
          modify $ CM.adjust (\(uses,attempts)->(uses+wt,attempts)) c
        countUses (CApp{lComb=l, rComb=r}) wt tyLib = do
          countUses l wt tyLib >> countUses r wt tyLib
        countUses _ _ _ = return ()
        -- Counts number of expansions used in the combinator
        countExpansions :: Comb -> Double -> State (Double, Double) ()
        countExpansions c@(CApp{lComb = l, rComb = r}) wt | not (CM.member c lib) = do
          modify $ \(expansions, total) -> (expansions+wt,total+wt)
          countExpansions l wt
          countExpansions r wt
        countExpansions _ wt = do
          modify $ \(expansions, total) -> (expansions,total+wt)
          
  
  
  
  

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
          logProbAll = log $ sum combLps
          combLogProb = (library gr) CM.! c - logProbAll
          out = if length altCs < 2 then log (0.5) else combLogProb
      in out

combinatorLL :: Grammar
                -> Comb
                -> Double
-- | Returns the log probability of producing the given tree
-- when asked for the given type
combinatorLL gr c =
  -- Is this combinator a leaf?
  if CM.member c (library gr)
  then calcLogProb gr (cReqType c) c +
       (log $ 1 + (negate $ exp $ negate $ expansions gr))
  else case c of
    CApp{lComb = l, rComb = r} ->
      combinatorLL gr l + combinatorLL gr r - (expansions gr)
    _ -> -- Not in library => Expansion => CApp
      error $ "Combinator "++show c++" is not an application, and is not in library"

      
      

-- exLogProb :: Grammar -> Type -> Double
-- exLogProb gr tp  
--     = let m = filterCombinatorsByType (CM.keys $ library gr) tp
--           altCs = map fst $ runStateT m 0
--           combLps = [exp $ (library gr) CM.! x | x <- altCs] 
--           logProbAll = log $ exp (expansions gr) + sum combLps
--           out = if null altCs then log (0.5)  -- ^ this log (0.5) is a
--                                               -- hack. it should be 0,
--                                               -- right? since there
--                                               -- are no alternatives,
--                                               -- the probability is
--                                               -- 1. But this causes
--                                               -- infinite expansions
--                                               -- in the best-first search. 
--                 else expansions gr  - logProbAll
--       in  out




truncateGrammar :: Grammar -> Int -> Grammar
truncateGrammar (Grammar lib _) n = Grammar lib' 0
    where xs = CM.toList lib
          xs' = take n $ sortBy (\(_, y) (_, x) -> x `compare` y) xs
          lib' = CM.fromList xs'
          
    
                                
                                       
                      
    

          
    
    