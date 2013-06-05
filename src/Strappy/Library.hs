{-# Language TypeSynonymInstances, FlexibleInstances #-}

module Strappy.Library where

import Data.Maybe 
import qualified Data.Map as Map hiding ((\\))
import Data.Hashable
import GHC.Prim
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.List as List
import Data.List ((\\))
import Text.Printf
import Data.Function (on)
import Control.Monad.Identity
import Control.Monad.State
import Control.Arrow (first)
import Debug.Trace

import Strappy.Type
import Strappy.Expr
import Strappy.Utils

-- | Type alias for hash table with keys as type-hidden expressions.
type ExprMap a = Map.Map Expr a

-- | Type alias for distribution over expressions. 
type ExprDistr = ExprMap Double 

showExprDistr exprDistr  = unlines $ map (\(e, i) -> printf "%7s:%7.2f" (show e) i) pairs
    where pairs = List.sortBy (compare `on` snd) $ Map.toList exprDistr
                  
showExprDistrLong exprDistr = unlines $ map (\(e, i) -> printf "%60s, %7.2f" (showExprLong e) i) pairs
    where pairs = List.sortBy (compare `on` snd) $ Map.toList exprDistr

-- | Type for stochastic grammar over programs.
data Grammar = Grammar {grApp :: Double, -- ^ log probability of application
                        grExprDistr :: ExprDistr -- ^ distribution over functions
                       } 

showGrammar (Grammar p exprDistr) = printf "%7s:%7.2f\n" "p" (exp p) ++ showExprDistr exprDistr

normalizeGrammar :: Grammar -> Grammar 
normalizeGrammar Grammar{grApp=p, grExprDistr=distr}
    = let logTotalMass = logSumExpList $ p : Map.elems distr
          distr' = Map.map (\x -> x - logTotalMass) distr
          p' = p - logTotalMass
      in Grammar  p' distr'

-- | Methods for calculating the loglikelihood of an expression draw from grammar
-- This is very slow, because all of the alternatives have to be annotated
exprLogLikelihood :: Grammar -> Expr -> Double
-- | Returns the log probability of producing the given expr tree
exprLogLikelihood gr = fromJust . eLogLikelihood . annotateLogLikelihoods gr . annotateAlternatives gr

-- | Given a library, finds all of the alternative productions we could have used
-- Assumes that the eReqType fields have already been filled in
annotateAlternatives :: Grammar -> Expr -> Expr
annotateAlternatives gr@(Grammar{grExprDistr = distr}) e@Term{ eReqType = Just tp } =
  e { eAlternatives = Just $
                      map fst $
                      filter (\(e, _) -> canUnifyFast (eType e) tp) $
                      Map.toList distr }
annotateAlternatives gr@(Grammar{grExprDistr = distr}) e@App{ eLeft = left, eRight = right, eReqType = Just tp } =
  e { eLeft = annotateAlternatives gr left,
      eRight = annotateAlternatives gr right,
      eAlternatives =
        if Map.member e distr
        then Just $
             map fst $
             filter (\(e, _) -> canUnifyFast (eType e) tp) $
             Map.toList distr 
        else Nothing }
annotateAlternatives _ _ = error "Annotating alternatives on expression w/o eReqType annotated"

-- | Updates the alternative productions when the library changes a small amount
updateAlternatives :: [Expr] -> -- ^ added to library
                      [Expr] -> -- ^ removed from library
                      Grammar -> -- ^ new library
                      Expr -> -- ^ expression annotated w/ old library
                      Expr -- ^ expression annotated w/ new library
updateAlternatives add del _ e@Term{ eReqType = Just tp, eAlternatives = Just alts } =
  e { eAlternatives = Just $ (alts \\ del) ++ filter (canUnifyFast tp . eType) add }
updateAlternatives add del gr e@App{ eReqType = Just tp,
                                     eAlternatives = Just alts,
                                     eLeft = left,
                                     eRight = right } =
  if e `elem` del -- Did we remove e from the library?
  then e { eAlternatives = Nothing,
           eLeft  = updateAlternatives add del gr left,
           eRight = updateAlternatives add del gr right }
  else e { eAlternatives = Just $ (alts \\ del) ++ filter (canUnifyFast tp . eType) add,
           eLeft  = updateAlternatives add del gr left,
           eRight = updateAlternatives add del gr right }
updateAlternatives add del gr@(Grammar{ grExprDistr = distr}) e@App{ eReqType = Just tp,
                                                                     eAlternatives = Nothing,
                                                                     eLeft = left,
                                                                     eRight = right } =
  if e `elem` add -- Did we add e to the library?
  then let newProductions = (add ++ map fst (Map.toList distr)) \\ del
       in e { eAlternatives = Just $ filter (canUnifyFast tp . eType) newProductions,
              eLeft  = updateAlternatives add del gr left,
              eRight = updateAlternatives add del gr right }
  else e { eLeft  = updateAlternatives add del gr left,
           eRight = updateAlternatives add del gr right }
                  


-- | Fills in the eLogLikelihood fields of the tree
-- Assumes that the eAlternatives fields have already been filled in
annotateLogLikelihoods :: Grammar -> Expr -> Expr
annotateLogLikelihoods gr expr@App{ eLeft = l, eRight = r, eAlternatives = alts } =
  let l' = annotateLogLikelihoods gr l
      r' = annotateLogLikelihoods gr r
      l'LL = fromJust $ eLogLikelihood l'
      r'LL = fromJust $ eLogLikelihood r'
      appLL = grApp gr + l'LL + r'LL
      ll = if isJust alts -- alts is Nothing if there were no alts, eg, no production used
           then logSumExp appLL
                (calcLogProb gr expr + log (1 - exp (grApp gr)))
           else appLL
  in expr { eLogLikelihood = Just ll,
            eLeft = l',
            eRight = r' }
annotateLogLikelihoods gr expr@Term{ } =
  let grLL = calcLogProb gr expr 
      ll = grLL + log (1 - exp (grApp gr))
  in expr { eLogLikelihood = Just ll }

-- | Annotates the requested types
-- Takes as input the top-level type request
annotateRequestedM :: Monad m =>
                     Type -> -- ^ Requested type of the expression
                     Expr -> -- ^ The expression
                     TypeInference m Expr -- ^ The freshly annotated expression
annotateRequestedM tp e@(App { eLeft = l, eRight = r }) = do
  t <- mkTVar
  l' <- annotateRequestedM (t ->- tp) l
  t' <- applySub t
  r' <- annotateRequestedM t' r
  tp' <- applySub tp
  return e { eLeft = l', eRight = r', eType = tp', eReqType = Just tp }
annotateRequestedM tp e@(Term { eType = eTp }) = do
  eTp' <- instantiateType eTp
  unify tp eTp'
  return $ e { eReqType = Just tp }

-- | Non-monadic wrapper
-- Presumes no constraint on top-level type
annotateRequested :: Expr -> Expr
annotateRequested expr = runIdentity $ runTI $ do
  tp <- mkTVar
  annotateRequestedM tp expr

-- | Non-monadic wrapper that allows one to specify the type
annotateRequested' :: Type -> Expr -> Expr
annotateRequested' tp expr = runIdentity $ runTI $ do
  tp' <- instantiateType tp
  annotateRequestedM tp' expr



-- | Returns the log probability of using a given expression from the library of
-- terminals.
-- Assumes that eAlternatives has been filled in.
calcLogProb :: Grammar -> Expr -> Double
calcLogProb gr@(Grammar { grExprDistr = distr}) expr | isJust (eAlternatives expr) =
  let logp_e = distr Map.! expr
      logp_e_tot = logSumExpList (map (distr Map.!) $ fromJust $ eAlternatives expr) 
    in logp_e - logp_e_tot
calcLogProb _ _ = error "Attempt to calcLogProb of an expression w/o eAlternatives filled in."

data Counts = Counts {appCounts :: Double,
                        termCounts :: Double,
                        useCounts :: ExprMap Double,
                        possibleUseCounts :: ExprMap Double} deriving Show
estimateGrammar :: Grammar 
                -> Double -- pseudocount by which to weight the grammar as a prior
                -> [(Expr, Double)] -- weighted observations 
                -> Grammar
estimateGrammar Grammar{grExprDistr=distr} pseudocounts obs
  = Grammar{grApp=appLogProb, grExprDistr=distr'}
    where es = Map.toList distr -- [(Expr, Double)]
          -- Accumulator function that takes a current records of counts and an expression and undates the counts.
          go :: Counts -> Expr -> Double -> Counts
          -- If expression is a terminal. 
          go (Counts ac tc uc pc) expr weight | Map.member expr distr =
            let tp = fromJust $ eReqType expr
                tc' = tc + weight
                uc' = Map.adjust (+ weight) (findLibExpr expr) uc
                otherTerms = map fst $ filter (\(e,_) -> canUnifyFast (eType e) tp) es
                pc' = List.foldl' (Prelude.flip $ Map.adjust (+ weight)) pc otherTerms 
            in Counts ac tc' uc' pc'
          go counts@(Counts ac tc uc pc) expr@App{eRight=r, eLeft=l} weight =
            let countsLeft = go counts l weight
                countsRight = go countsLeft r weight
            in countsRight { appCounts = appCounts countsRight + weight } 
          empty = Map.map (const pseudocounts) distr
          counts = List.foldl' (\cts (e, w) -> go cts e w) (Counts pseudocounts pseudocounts empty empty) obs
          appLogProb = log (appCounts counts) - log (termCounts counts + appCounts counts)
          distr' = Map.unionWith (\a a' -> log a - log a') (useCounts counts) (possibleUseCounts counts)
          findLibExpr expr = fst $ fromJust $ List.find (\(e,_) -> e==expr) es


-- | Iteratively performs the inside-out algorithm to a corpus, restimating the gramar
-- This assumes that the corpus has already had its alternate productions annotated.
iterateInOut :: Int -> Grammar -> Double -> [(Expr, Double)] -> Grammar
iterateInOut k g prior obs =
  foldl (\g' _ -> inoutEstimateGrammar g' prior obs) g [1..k]

-- | Uses the inside-out algorithm to find the production probabilities
-- TODO: I don't think that findLibExpr is needed here.
-- Either way, it eventually won't be, once we transition to a data structure that allows us to query keys.
inoutEstimateGrammar :: Grammar -> Double -> [(Expr, Double)] -> Grammar
inoutEstimateGrammar gr@Grammar{grExprDistr=distr, grApp = app} pseudocounts obs =
  Grammar{grApp = appLogProb, grExprDistr = distr'}
  where es = Map.toList distr -- [(Expr, Double)]
        expectedCounts :: Double -> Counts -> Expr -> Counts
        expectedCounts weight counts expr@(Term { eAlternatives = Just otherTerms }) =
          let uc' = Map.adjust (+weight) (findLibExpr expr) (useCounts counts)
              otherTerms' = map findLibExpr otherTerms
              pc' = List.foldl' (Prelude.flip $ Map.adjust (+ weight)) (possibleUseCounts counts) otherTerms'
          in counts { termCounts = termCounts counts + weight,
                      useCounts = uc',
                      possibleUseCounts = pc' }
        expectedCounts weight counts expr@(App { eLeft = left,
                                                 eRight = right,
                                                 eAlternatives = Just otherTerms }) =
          let logProbLib = calcLogProb gr expr + log (1 - exp app)
              logProbApp = app + fromJust (eLogLikelihood left) + fromJust (eLogLikelihood right)
              probUsedApp = exp $ logProbApp - logSumExp logProbApp logProbLib
              probUsedLib = 1 - probUsedApp
              uc' = Map.adjust (+(weight*probUsedLib)) (findLibExpr expr) (useCounts counts)
              otherTerms' = map findLibExpr otherTerms
              pc' = List.foldl' (Prelude.flip $ Map.adjust (+ (weight*probUsedLib))) (possibleUseCounts counts) otherTerms'
              counts' = counts { appCounts = appCounts counts + weight * probUsedApp,
                                 termCounts = termCounts counts + weight * probUsedLib, 
                                 useCounts = uc',
                                 possibleUseCounts = pc' }
              counts'' = expectedCounts (weight * probUsedApp) counts' left
              counts''' = expectedCounts (weight * probUsedApp) counts'' right
          in counts'''
        expectedCounts weight counts expr@(App { eLeft = left,
                                                 eRight = right,
                                                 eAlternatives = Nothing }) =
          let counts'   = counts { appCounts = appCounts counts + weight }
              counts''  = expectedCounts weight counts' left
              counts''' = expectedCounts weight counts'' right
          in counts'''
        empty = Map.map (const pseudocounts) distr
        obs' = map (\(e,w) -> (annotateLogLikelihoods gr e, w)) obs
        counts = List.foldl' (\cts (e, w) -> expectedCounts w cts e) (Counts pseudocounts pseudocounts empty empty) obs'
        appLogProb = log (appCounts counts) - log (termCounts counts + appCounts counts)
        distr' = Map.unionWith (\a a' -> log a - log a') (useCounts counts) (possibleUseCounts counts)
        findLibExpr expr = fst $ fromJust $ List.find (\(e,_) -> e==expr) es


---------------------------------------------------------------------      

-- | Helper for turning a Haskell type to Any. 
mkAny :: a -> Any
mkAny = unsafeCoerce  

-- | Basic combinators
cI = mkTerm "I" (t ->- t)   id

cS = mkTerm "S" ((t2 ->- t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t)    $ \f g x -> f x (g x)

cB = mkTerm "B" ((t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t)   $ \f g x -> f (g x)

cC = mkTerm "C" ((t1 ->- t2 ->- t) ->- t2 ->- t1 ->- t)   $ \f g x -> f x g 

cK = mkTerm "K" (t1 ->- t2 ->- t1)   $ \x y -> x




-- | Integer arithmetic
cPlus :: Expr
cPlus = mkTerm "+" (tInt ->- tInt ->- tInt)   (+)

cTimes :: Expr
cTimes = mkTerm "*" (tInt ->- tInt ->- tInt)   (*)

cMinus :: Expr
cMinus = mkTerm "-" (tInt ->- tInt ->- tInt)   (-)

-- What is this supposed to be?
cMod :: Expr
cMod = mkTerm "-" (tInt ->- tInt ->- tInt)    (-)

cRem :: Expr
cRem = mkTerm "rem" (tInt ->- tInt ->- tInt)   mod
-- | Lists
cCons = mkTerm ":"  (t ->- tList t ->- tList t)    (:)
cAppend = mkTerm "++" (tList t ->- tList t ->- tList t)   (++)
cHead = mkTerm "head" (tList t ->- t)   head
cMap = mkTerm "map" ((t ->- t1) ->- tList t ->- tList t1)   map
cEmpty = mkTerm "[]" (tList t)   []
cSingle = mkTerm "single" (t ->- tList t)   $ replicate 1 
cRep = mkTerm "rep" (tInt ->- t ->- tList t)   replicate 
cFoldl = mkTerm "foldl" ((t ->- t1 ->- t) ->- t ->- tList t1 ->- t)    List.foldl'
cInts =  [cInt2Expr i | i <- [1..10]]
cDoubles =  [cDouble2Expr i | i <- [1..10]]

-- | A basic collection of expressions
basicExprs :: [Expr]
basicExprs = [cI, 
              cS, 
              cB, 
              cC, 
              cK, 
--              cBottom, -- Why would we put cBottom in to the library?
              cPlus,
              cTimes, 
              cCons, 
              cEmpty,
              cAppend,
              --         cHead,
              cMap,
              cFoldl,
              cSingle,
              cRep
             ] 
             ++ cInts

mkExprDistr :: [Expr] -> ExprDistr
mkExprDistr exprs = Map.adjust (const (-5)) cBottom
                    $ Map.fromList [(e, 1) | e <- exprs] 

 
-- | A basic expression distribution
basicExprDistr :: ExprDistr
basicExprDistr = Map.adjust (const (-5)) cBottom
                 $ Map.fromList [(e, 1) | e <- basicExprs] 
                 


basicGrammar :: Grammar
basicGrammar = normalizeGrammar $ Grammar 3 basicExprDistr

-- | Helpers 
-- | compose epressions together
compose :: [Expr] -> Expr
compose = foldl1 (<>)

clearGrammarProbs :: Grammar -> Grammar
clearGrammarProbs Grammar { grExprDistr = distr } =
  Grammar { grExprDistr = Map.map (const $ log 0.5) distr,
            grApp = log 0.5 }


-- | Initializing a TypeInference monad with a Library. We need to
-- grab all type variables in the library and make sure that the type
-- variable counter in the state of the TypeInference monad is greater
-- that that counter.
initializeTI :: Monad m => ExprDistr -> TypeInference m ()
initializeTI exprDistr = modify $ \(_, s) -> (i+1, s)
    where i = maximum $
              concatMap (getTVars . eType . fst) $
              Map.toList exprDistr

