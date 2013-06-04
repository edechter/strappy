{-# Language TypeSynonymInstances, FlexibleInstances #-}

module Strappy.Library where

import Data.Maybe 
import qualified Data.HashMap as Map
import Data.Hashable
import GHC.Prim
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.List as List
import Text.Printf
import Data.Function (on)
import Control.Monad.Identity
import Control.Arrow (first)

import Strappy.Type
import Strappy.Expr
import Strappy.Utils

-- | Type alias for hash table with keys as type-hidden expressions.
type ExprMap a = Map.Map UExpr a

instance Hashable UExpr where
    hashWithSalt a  uexpr = hash a `hashWithSalt` hash (fromUExpr uexpr)


-- | Type alias for distribution over expressions. 
type ExprDistr = ExprMap Double 

showExprDistr exprDistr  = unlines $ map (\(e, i) -> printf "%7s:%7.2f" (show e) i) pairs
    where pairs = List.sortBy (compare `on` snd) $ Map.toList exprDistr
                  
showExprDistrLong exprDistr = unlines $ map (\(e, i) -> printf "%60s, %7.2f" (showUExprLong e) i) pairs
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
exprLogLikelihood :: Grammar -> Expr a -> Double
-- | Returns the log probability of producing the given expr tree
exprLogLikelihood gr = fromJust . eLogLikelihood . annotateLogLikelihoods gr

-- | Fills in the eLogLikelihood fields of the tree
annotateLogLikelihoods :: Grammar -> Expr a -> Expr a
annotateLogLikelihoods gr expr = let e = toUExpr expr in
  case (expr, Map.member e (grExprDistr gr)) of
    (App{eLeft=l, eRight=r}, inGr) ->
      let l' = annotateLogLikelihoods gr l
          r' = annotateLogLikelihoods gr r
          l'LL = fromJust $ eLogLikelihood l'
          r'LL = fromJust $ eLogLikelihood r'
          ll = if inGr
               then logSumExp (l'LL + r'LL + grApp gr)
                              (calcLogProb gr expr + log (1 - exp (grApp gr)))
               else grApp gr + l'LL + r'LL
      in App { eLogLikelihood = Just ll,
               eType = eType expr,
               eReqType = eReqType expr,
               eLabel = eLabel expr,
               eLeft = fromUExpr (toUExpr l'),
               eRight = fromUExpr (toUExpr r') }
    (Term{}, _) ->
      let ll = calcLogProb gr expr + log (1 - exp (grApp gr))
      in expr { eLogLikelihood = Just ll }


-- | Returns the log probability of using a given expression from the library of
-- terminals.
calcLogProb :: Grammar -> Expr a -> Double
calcLogProb gr@Grammar{grExprDistr=distr} expr
  = let tp = fromJust (eReqType expr)
        m = filter (\(e, _) -> canUnify (eType $ fromUExpr e) tp) $ Map.toList distr
        logp_e = distr Map.! toUExpr expr
        logp_e_tot = logSumExpList (map snd m) 
    in  logp_e - logp_e_tot

data Counts = Counts {appCounts :: Double,
                        termCounts :: Double,
                        useCounts :: ExprMap Double,
                        possibleUseCounts :: ExprMap Double} deriving Show
estimateGrammar :: Grammar 
                -> Double -- pseudocount by which to weight the grammar as a prior
                -> [(UExpr, Double)] -- weighted observations 
                -> Grammar
estimateGrammar Grammar{grExprDistr=distr} pseudocounts obs
  = Grammar{grApp=appLogProb, grExprDistr=distr'}
    where es = Map.toList distr -- [(UExpr, Double)]
          -- Accumulator function that takes a current records of counts and an expression and undates the counts.
          go :: Counts -> Expr a -> Double -> Counts
          -- If expression is a terminal. 
          go (Counts ac tc uc pc) expr weight | Map.member (toUExpr expr) distr =
            let tp = fromJust $ eReqType expr
                tc' = tc + weight
                uc' = Map.adjust (+ weight) (findLibExpr $ toUExpr expr) uc
                otherTerms = map fst $ filter (\(e,_) -> canUnify (eType $ fromUExpr e) tp) es
                pc' = List.foldl' (Prelude.flip $ Map.adjust (+ weight)) pc otherTerms 
            in Counts ac tc' uc' pc'
          go counts@(Counts ac tc uc pc) expr@App{eRight=r, eLeft=l} weight =
            let countsLeft = go counts l weight
                countsRight = go countsLeft r weight
            in countsRight { appCounts = appCounts countsRight + weight } 
          empty = Map.map (const pseudocounts) distr
          counts = List.foldl' (\cts (e, w) -> go cts (fromUExpr e) w) (Counts pseudocounts pseudocounts empty empty) obs
          appLogProb = log (appCounts counts) - log (termCounts counts + appCounts counts)
          distr' = Map.unionWith (\a a' -> log a - log a') (useCounts counts) (possibleUseCounts counts)
          findLibExpr expr = fst $ fromJust $ List.find (\(e,_) -> e==expr) es


iterateInOut :: Int -> Grammar -> Double -> [(UExpr, Double)] -> Grammar
iterateInOut 0 g _ _ = g
iterateInOut k g prior obs =
  iterateInOut (k-1) (inoutEstimateGrammar g prior obs) prior obs

-- | Uses the in-out algorithm to find the production probabilities
inoutEstimateGrammar :: Grammar -> Double -> [(UExpr, Double)] -> Grammar
inoutEstimateGrammar gr@Grammar{grExprDistr=distr, grApp = app} pseudocounts obs =
  Grammar{grApp = appLogProb, grExprDistr = distr'}
  where es = Map.toList distr -- [(UExpr, Double)]
        expectedCounts :: Double -> Counts -> Expr a -> Counts
        expectedCounts weight counts expr@(Term { eReqType = (Just tp) }) =
          let uc' = Map.adjust (+weight) (findLibExpr $ toUExpr expr) (useCounts counts)
              otherTerms = map fst $ filter (\(e,_) -> canUnify (eType $ fromUExpr e) tp) es
              pc' = List.foldl' (Prelude.flip $ Map.adjust (+ weight)) (possibleUseCounts counts) otherTerms
          in counts { termCounts = termCounts counts + weight,
                      useCounts = uc',
                      possibleUseCounts = pc' }
        expectedCounts weight counts expr@(App { eLeft = left,
                                                 eRight = right,
                                                 eReqType = (Just tp) }) =
          if Map.member (toUExpr expr) distr
          then let logProbLib = calcLogProb gr expr + log (1 - exp app)
                   logProbApp = app + fromJust (eLogLikelihood left) + fromJust (eLogLikelihood right)
                   probUsedApp = exp $ logProbApp - logSumExp logProbApp logProbLib
                   probUsedLib = 1 - probUsedApp
                   uc' = Map.adjust (+(weight*probUsedLib)) (findLibExpr $ toUExpr expr) (useCounts counts)
                   otherTerms = map fst $ filter (\(e,_) -> canUnify (eType $ fromUExpr e) tp) es
                   pc' = List.foldl' (Prelude.flip $ Map.adjust (+ (weight*probUsedLib))) (possibleUseCounts counts) otherTerms
                   counts' = counts { appCounts = appCounts counts + weight * probUsedApp,
                                      termCounts = termCounts counts + weight * probUsedLib, 
                                      useCounts = uc',
                                      possibleUseCounts = pc' }
                   counts'' = expectedCounts (weight * probUsedApp) counts' left
                   counts''' = expectedCounts (weight * probUsedApp) counts'' right
               in counts'''
          else let counts'   = counts { appCounts = appCounts counts + weight }
                   counts''  = expectedCounts weight counts' left
                   counts''' = expectedCounts weight counts'' right
               in counts'''
        empty = Map.map (const pseudocounts) distr
        obs' = map (\(e,w) -> (toUExpr $ annotateLogLikelihoods gr $ fromUExpr e, w)) obs
        counts = List.foldl' (\cts (e, w) -> expectedCounts w cts (fromUExpr e)) (Counts pseudocounts pseudocounts empty empty) obs'
        appLogProb = log (appCounts counts) - log (termCounts counts + appCounts counts)
        distr' = Map.unionWith (\a a' -> log a - log a') (useCounts counts) (possibleUseCounts counts)
        findLibExpr expr = fst $ fromJust $ List.find (\(e,_) -> e==expr) es


---------------------------------------------------------------------      

-- | Helper for turning a Haskell type to Any. 
mkAny :: a -> Any
mkAny = unsafeCoerce  

--  Some basic library entires. 
t = TVar 0                  
t1 = TVar 1               
t2 = TVar 2                  
t3 = TVar 3                  

-- | Basic combinators
cI = mkTerm "I" (t ->- t)   id

cS = mkTerm "S" ((t2 ->- t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t)    $ \f g x -> f x (g x)

cB = mkTerm "B" ((t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t)   $ \f g x -> f (g x)

cC = mkTerm "C" ((t1 ->- t2 ->- t) ->- t2 ->- t1 ->- t)   $ \f g x -> f x g 

cK = mkTerm "K" (t1 ->- t2 ->- t1)   $ \x y -> x




-- | Integer arithmetic
cPlus :: Expr (Int -> Int -> Int)
cPlus = mkTerm "+" (tInt ->- tInt ->- tInt)   (+)

cTimes :: Expr (Int -> Int -> Int)
cTimes = mkTerm "*" (tInt ->- tInt ->- tInt)   (*)

cMinus :: Expr (Int -> Int -> Int)
cMinus = mkTerm "-" (tInt ->- tInt ->- tInt)   (-)

-- What is this supposed to be?
cMod :: Expr (Int -> Int -> Int)
cMod = mkTerm "-" (tInt ->- tInt ->- tInt)    (-)

cRem :: Expr (Int -> Int -> Int)
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
basicExprs :: [UExpr]
basicExprs = [toUExpr cI, 
              toUExpr cS, 
              toUExpr cB, 
              toUExpr cC, 
              toUExpr cK, 
--              toUExpr cBottom, -- Why would we put cBottom in to the library?
              toUExpr cPlus,
              toUExpr cTimes, 
              toUExpr cCons, 
              toUExpr cEmpty,
              toUExpr cAppend,
              --         toUExpr cHead,
              toUExpr cMap,
              toUExpr cFoldl,
              toUExpr cSingle,
              toUExpr cRep
             ] 
             ++ map toUExpr cInts

mkExprDistr :: [UExpr] -> ExprDistr
mkExprDistr exprs = Map.adjust (const (-5)) (toUExpr cBottom) 
                    $ Map.fromList [(e, 1) | e <- exprs] 

 
-- | A basic expression distribution
basicExprDistr :: ExprDistr
basicExprDistr = Map.adjust (const (-5)) (toUExpr cBottom) 
                 $ Map.fromList [(e, 1) | e <- basicExprs] 
                 


basicGrammar :: Grammar
basicGrammar = normalizeGrammar $ Grammar 3 basicExprDistr

-- | Helpers 
-- | compose epressions together
compose :: [UExpr] -> UExpr
compose (x:[]) = x
compose (x:xs) = toUExpr $ fromUExpr x <> fromUExpr (compose xs) 
compose []  = error "compose: applied to empty list"

clearGrammarProbs :: Grammar -> Grammar
clearGrammarProbs Grammar { grExprDistr = distr } =
  Grammar { grExprDistr = Map.map (const $ log 0.5) distr,
            grApp = log 0.4 }