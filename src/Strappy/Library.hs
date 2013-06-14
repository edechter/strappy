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
import Strappy.Config

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
-- If usePCFGWeighting is false, then requested types should be annotated before invoking this procedure
-- Returns the same expression, but with log likelihoods annotated
exprLogLikelihood :: Grammar -> Expr -> Expr
exprLogLikelihood gr e = if usePCFGWeighting
                         then pcfgLogLikelihood gr e
                         else ijcaiLogLikelihood gr e

-- | Returns the log probability of producing the given expr tree
pcfgLogLikelihood :: Grammar -> Expr -> Expr
pcfgLogLikelihood (Grammar { grExprDistr = distr }) e@(Term { }) = e { eLogLikelihood = Just (distr Map.! e) }
pcfgLogLikelihood gr@(Grammar { grExprDistr = distr, grApp = app }) e@(App { eLeft = l, eRight = r }) =
  let l' = pcfgLogLikelihood gr l
      r' = pcfgLogLikelihood gr r
      lLL = fromJust $ eLogLikelihood l'
      rLL = fromJust $ eLogLikelihood r'
      eLL = logSumExp (app + lLL + rLL)
                      (case Map.lookup e distr of
                          Nothing -> log 0.0
                          Just p -> p)
  in e { eLeft = l', eRight = r', eLogLikelihood = Just eLL }

-- | Annotates log likelihood as done in IJCAI paper
ijcaiLogLikelihood :: Grammar -> Expr -> Expr
ijcaiLogLikelihood (Grammar { grApp = logApp, grExprDistr = distr }) e@(Term { eReqType = Just tp}) =
  let alts = filter (\(e', _) -> canUnifyFast tp (eType e')) $ Map.toList distr
      z = logSumExpList $ map snd alts
      logTerm = log (1 - exp logApp)
      ll = (distr Map.! e) + logTerm - z
  in e { eLogLikelihood = Just ll }
ijcaiLogLikelihood gr (Term { eReqType = Nothing }) =
  error "ijcaiLogLikelihood called on Term without requested types annotated"
ijcaiLogLikelihood gr@(Grammar { grApp = logApp, grExprDistr = distr }) e@(App { eLeft = l,
                                                                                eRight = r,
                                                                                eReqType = Just tp})
  | Map.member e distr =
    let alts = filter (\(e, _) -> canUnifyFast tp (eType e)) $ Map.toList distr
        z = logSumExpList $ map snd alts
        logTerm = log (1 - exp logApp)
        l' = ijcaiLogLikelihood gr l
        r' = ijcaiLogLikelihood gr r
        lLL = fromJust $ eLogLikelihood l'
        rLL = fromJust $ eLogLikelihood r'
        eLL = logSumExp ((distr Map.! e) + logTerm - z)
                        (lLL + rLL + logApp)
    in e { eLeft = l', eRight = r', eLogLikelihood = Just eLL }
ijcaiLogLikelihood _ (App { eReqType = Nothing }) =
  error "ijcaiLogLikelihood called on App without requested types annotated"
ijcaiLogLikelihood gr@(Grammar { grApp = logApp }) e@(App { eLeft = l, eRight = r}) =
  let l' = ijcaiLogLikelihood gr l
      r' = ijcaiLogLikelihood gr r
      lLL = fromJust $ eLogLikelihood l'
      rLL = fromJust $ eLogLikelihood r'
      eLL = logApp + lLL + rLL
  in e { eLeft = l', eRight = r', eLogLikelihood = Just eLL }

  

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



data Counts = Counts {appCounts :: Double,
                        termCounts :: Double,
                        useCounts :: ExprMap Double,
                        possibleUseCounts :: ExprMap Double} deriving Show

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
        -- Updates expected counts
        expectedCounts :: Double -> Counts -> Expr -> Counts
        expectedCounts weight counts expr@(Term { eReqType = Just tp }) =
          let uc' = Map.insertWith (+) expr weight $ useCounts counts
              alts = filter (\(e', _) -> canUnifyFast tp (eType e')) es
              pc = possibleUseCounts counts
              pc' = foldl (\acc alt -> Map.insertWith (+) alt weight acc) pc $ map fst alts
              logZ = if usePCFGWeighting then 0.0 else logSumExpList (map snd alts)
              counts' = counts { termCounts = termCounts counts + weight,
                                 useCounts = uc', 
                                 possibleUseCounts = pc' }
          in counts'
        expectedCounts weight counts expr@(App { eLeft = left,
                                                 eRight = right, 
                                                 eReqType = Just tp }) | Map.member expr distr =
          let alts = filter (\(e', _) -> canUnifyFast tp (eType e')) es
              logZ = if usePCFGWeighting then 0.0 else logSumExpList (map snd alts)
              leftLL  = fromJust $ eLogLikelihood left
              rightLL = fromJust $ eLogLikelihood right
              -- Find probability of having used an application vs a library procedure
              logProbLib = distr Map.! expr + log (1 - exp app) - logZ
              logProbApp = app + leftLL + rightLL
              probUsedApp = exp $ logProbApp - logSumExp logProbApp logProbLib
              probUsedLib = 1 - probUsedApp
              -- Recurse on children
              counts'  = expectedCounts (weight*probUsedApp) counts left
              counts'' = expectedCounts (weight*probUsedApp) counts right
              -- Add in counts for if we used a library procedure
              uc' = Map.insertWith (+) expr (weight*probUsedLib) $ useCounts counts''
              pc  = possibleUseCounts counts''
              pc' = foldl (\acc alt -> Map.insertWith (+) alt (weight*probUsedLib) acc) pc $ map fst alts
              counts''' = counts'' { appCounts = appCounts counts'' + weight * probUsedApp,
                                     termCounts = termCounts counts'' + weight * probUsedLib, 
                                     useCounts = uc', 
                                     possibleUseCounts = pc' }
              
          in counts'''
        expectedCounts weight counts expr@(App { eLeft = left,
                                                 eRight = right, 
                                                 eReqType = Just _}) =
           let counts'   = counts { appCounts = appCounts counts + weight }
               counts''  = expectedCounts weight counts' left
               counts''' = expectedCounts weight counts'' right
          in counts'''
        obs' = map (\(e,w) -> (exprLogLikelihood gr e, w)) obs
        counts = List.foldl' (\cts (e, w) -> expectedCounts w cts e) (Counts pseudocounts pseudocounts Map.empty Map.empty) obs'
        uses' = List.foldl' (\cts e -> Map.insertWith (+) e pseudocounts cts) (useCounts counts) $ Map.keys distr
        possibleUses' = List.foldl' (\cts e -> Map.insertWith (+) e pseudocounts cts)
                                    (possibleUseCounts counts) $ Map.keys distr
        logTotalUses = log $ sum $ Map.elems uses'
        appLogProb = log (appCounts counts) - log (termCounts counts + appCounts counts)
        distr' = Map.mapWithKey (\expr _ ->
                                  if usePCFGWeighting
                                  then case Map.lookup expr uses' of
                                    Just u -> log u - logTotalUses
                                    Nothing -> error "Should never occur: expr not in uses or possible uses"
                                  else case (Map.lookup expr uses', Map.lookup expr possibleUses') of 
                                    (Just u, Just p) -> log u - log p
                                    _ -> error "Should never occur: expr not in uses or possible uses") distr



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
             ] ++ cInts

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