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
normalizeGrammar gr@Grammar{grApp=p, grExprDistr=distr} =
  let logTotalMass = logSumExpList $ Map.elems distr
      distr' = Map.map (\x -> x - logTotalMass) distr
  in gr { grExprDistr = distr' }

-- | Methods for calculating the loglikelihood of an expression draw from grammar
exprLogLikelihood :: Grammar -> Expr -> Double
-- | Returns the log probability of producing the given expr tree
exprLogLikelihood (Grammar { grExprDistr = distr }) e@(Term { }) = distr Map.! e
exprLogLikelihood gr@(Grammar { grExprDistr = distr, grApp = app }) e@(App { eLeft = l, eRight = r }) =
  logSumExp (app + exprLogLikelihood gr l + exprLogLikelihood gr r)
            (case Map.lookup e distr of
                Nothing -> log 0.0
                Just p -> p)
  

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
calcLogProb :: Grammar -> Expr -> Double
calcLogProb gr@(Grammar { grExprDistr = distr}) expr =
  let tp = safeFromJust "calcLogProb on expr w/o req types" $ eReqType expr
      alts = map snd $ filter (\(e, _) -> canUnifyFast (eType e) tp) $ Map.toList distr
      logp_e = distr Map.! expr
      logp_e_tot = logSumExpList alts
    in logp_e - logp_e_tot

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
-- This assumes we're sampling from P(program | typed)
iterateInOut :: Int -> Grammar -> Double -> [(Expr, Double)] -> Grammar
iterateInOut k g prior obs =
  foldl (\g' _ -> inoutEstimateGrammar g' prior obs) g [1..k]

-- | Uses the inside-out algorithm to find the production probabilities
inoutEstimateGrammar :: Grammar -> Double -> [(Expr, Double)] -> Grammar
inoutEstimateGrammar gr@Grammar{grExprDistr=distr, grApp = app} pseudocounts obs =
  Grammar{grApp = appLogProb, grExprDistr = distr'}
  where es = Map.toList distr -- [(Expr, Double)]
        expectedCounts :: Double -> Counts -> Expr -> Counts
        expectedCounts weight counts expr@(Term { }) =
          let uc' = Map.adjust (+weight) (findLibExpr expr) (useCounts counts)
              otherTerms = map fst es
              pc' = List.foldl' (Prelude.flip $ Map.adjust (+ weight)) (possibleUseCounts counts) otherTerms
          in counts { termCounts = termCounts counts + weight,
                      useCounts = uc',
                      possibleUseCounts = pc' }
        expectedCounts weight counts expr@(App { eLeft = left,
                                                 eRight = right }) | Map.member expr distr =
          let otherTerms = map fst es
              logProbLib = distr Map.! expr + log (1 - exp app)
              logProbApp = app + exprLogLikelihood gr left + exprLogLikelihood gr right
              probUsedApp = exp $ logProbApp - logSumExp logProbApp logProbLib
              probUsedLib = 1 - probUsedApp
              uc' = Map.adjust (+(weight*probUsedLib)) (findLibExpr expr) (useCounts counts)
              pc' = List.foldl' (Prelude.flip $ Map.adjust (+ (weight*probUsedLib))) (possibleUseCounts counts) otherTerms
              counts' = counts { appCounts = appCounts counts + weight * probUsedApp,
                                 termCounts = termCounts counts + weight * probUsedLib, 
                                 useCounts = uc',
                                 possibleUseCounts = pc' }
              counts'' = expectedCounts (weight * probUsedApp) counts' left
              counts''' = expectedCounts (weight * probUsedApp) counts'' right
          in counts'''
        expectedCounts weight counts expr@(App { eLeft = left,
                                                 eRight = right }) =
           let counts'   = counts { appCounts = appCounts counts + weight }
               counts''  = expectedCounts weight counts' left
               counts''' = expectedCounts weight counts'' right
          in counts'''
        empty = Map.map (const pseudocounts) distr
        counts = List.foldl' (\cts (e, w) -> expectedCounts w cts e) (Counts pseudocounts pseudocounts empty empty) obs
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

extendedGrammar :: Grammar
extendedGrammar = normalizeGrammar $ Grammar 3 $ Map.fromList $
                  [(e, 1) | e <- basicExprs] ++ [ (cK <> cI, 1), (cK <> (cInts!!2), 1) ]

-- | Helpers 
-- | compose epressions together
compose :: [Expr] -> Expr
compose = foldl1 (<>)

clearGrammarProbs :: Grammar -> Grammar
clearGrammarProbs Grammar { grExprDistr = distr } =
  Grammar { grExprDistr = Map.map (const $ log 0.5) distr,
            grApp = log 0.5 }

-- | Grammars are in CNF, so each production is the product of two other production
-- Strips out all productions that are subproductions of some other production
-- This is purely for the purpose of making it easier for humans to read grammars
removeSubProductions :: Grammar -> Grammar
removeSubProductions gr@Grammar{grExprDistr = distr} =
  let keys = Map.keys distr
      prods = filter (not . isTerm) keys
      subProductions = map eLeft prods ++ map eRight prods
      prods' = List.nub $ filter (not . Prelude.flip elem subProductions) prods
      prods'' = prods' ++ filter isTerm keys
  in gr { grExprDistr = Map.filterWithKey (\k v -> k `elem` prods'') distr }

-- | Initializing a TypeInference monad with a Library. We need to
-- grab all type variables in the library and make sure that the type
-- variable counter in the state of the TypeInference monad is greater
-- that that counter.
initializeTI :: Monad m => ExprDistr -> TypeInference m ()
initializeTI exprDistr = modify $ \(_, s) -> (i+1, s)
    where i = maximum $
              concatMap (getTVars . eType . fst) $
              Map.toList exprDistr





countSubtrees :: ExprMap Double -- <cnt>: Map of rules and associated counts.  
                 -> (Expr, Double) -- <expr>: the expression
                 -> ExprMap Double
countSubtrees cnt (expr@(App{eLeft=l, eRight=r}),wt) =
  let cnt'   = incCount cnt (expr,wt)
      cnt''  = countSubtrees cnt' (l,wt)
      cnt''' = countSubtrees cnt'' (r,wt)
  in
   cnt'''
countSubtrees cnt _ = cnt

incCount :: ExprMap Double -> (Expr, Double) 
            -> ExprMap Double
incCount cnt (expr@App{},wt) =
  Map.insertWith (+) expr wt cnt
incCount cnt _ = cnt

exprSize :: Expr -> Double
exprSize App{eLeft=l, eRight=r} = 1.0 + exprSize l + exprSize r
exprSize _ = 1.0