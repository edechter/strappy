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
import Debug.Trace

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

showGrammar (Grammar p exprDistr) = printf "%7s:%7.2f\n" "p" p ++ showExprDistr exprDistr

normalizeGrammar :: Grammar -> Grammar 
normalizeGrammar Grammar{grApp=p, grExprDistr=distr}
    = let logTotalMass = logsumexp $ p : Map.elems distr
          distr' = Map.map (\x -> x - logTotalMass) distr
          p' = p - logTotalMass
      in Grammar  p' distr'

-- | Methods for calculating the loglikelihood of an expression draw from grammar
exprLogLikelihood :: Grammar -> Expr a -> Double
-- | Returns the log probability of producing the given expr tree
exprLogLikelihood gr expr = let e = toUExpr expr in
    -- | Is this expr a leaf?
    -- trace ("\nExpr: " ++ showExprLong expr ++ "\n ExprDistr: " ++ showExprDistrLong (grExprDistr gr)) $ 
    if Map.member e (grExprDistr gr)
        then calcLogProb gr expr (fromMaybe (eType expr) (eReqType expr)) +
            log (1 - exp (grApp gr))
        else case expr of 
            App{eLeft=l, eRight=r} -> exprLogLikelihood gr l +
                                      exprLogLikelihood gr r +  
                                      grApp gr
            _  -> error $ "Expression "++show e++" is not an application, and is not in the library."

-- | Returns the probability of using a given expression from the library of
-- terminals.
calcLogProb :: Grammar -> Expr a -> Type -> Double
calcLogProb gr@Grammar{grExprDistr=distr} expr tp 
    | isTerm expr || isLabeled expr
    = let m = fst . runIdentity . runTI $ filterExprsByType (Map.toList distr) tp
          logp_e = distr Map.! toUExpr expr
          logp_e_tot = logsumexp (map snd m) 
      in  logp_e - logp_e_tot
    | otherwise = error "calcLogProb: the argument to calcLogProb must be either a Term or an App that is labeled."

data Counts = Counts {appCounts :: Double,
                        termCounts :: Double,
                        useCounts :: ExprMap Double,
                        possibleUseCounts :: ExprMap Double} deriving Show
estimateGrammar :: Grammar 
                -> Double -- pseudocount by which to weight the grammar as a prior
                -> [(UExpr, Double)] -- weighted observations 
                -> Grammar
estimateGrammar Grammar{grExprDistr=distr, grApp = app} pseudocounts obs 
    = Grammar{grApp=appLogProb, grExprDistr=distr'}
    where es = Map.toList distr -- [(UExpr, Double)]
          -- Accumulator function that takes a current records of counts and an expression and undates the counts.
          go :: Counts -> Expr a -> Double -> Counts
          -- If expression is a terminal. 
          go (Counts ac tc uc pc) expr@Term{eReqType=(Just tp)} weight =
            let tc' = tc + weight
                uc' = Map.adjust (+ weight) (toUExpr expr) uc
                otherTerms = map fst . fst . runIdentity . runTI $ filterExprsByType es tp
                pc' = List.foldl' (Prelude.flip $ Map.adjust (+ weight)) pc otherTerms 
            in Counts ac tc' uc' pc'
          go counts@(Counts ac tc uc pc) expr@App{eRight=r, eLeft=l} weight = let countsLeft = go counts l weight
                                                                                  countsRight = go countsLeft r weight
                                                                               in countsRight{appCounts= appCounts countsRight + weight} 
          empty = Map.map (const pseudocounts) distr
          counts = List.foldl' (\cts (e, w) -> go cts (fromUExpr e) w) (Counts pseudocounts pseudocounts empty empty) obs
          appLogProb = (trace $ show counts) $ log (appCounts counts) - log (termCounts counts + appCounts counts)
          distr' = Map.unionWith (\a a' -> log a - log a') (useCounts counts) (possibleUseCounts counts)
          
---------------------------------------------------------------------      

-- | Helper for turning a Haskell type to Any. 
mkAny :: a -> Any
mkAny = unsafeCoerce  

--  Some basic library entires. 
t = mkTVar 0                  
t1 = mkTVar 1               
t2 = mkTVar 2                  
t3 = mkTVar 3                  

-- | Basic combinators
cI = mkTerm "I" (t ->- t)   id

cS = mkTerm "S" ((t2 ->- t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t)    $ \f g x -> f x (g x)

cB = mkTerm "B" ((t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t)   $ \f g x -> f (g x)

cC = mkTerm "C" ((t1 ->- t2 ->- t) ->- t2 ->- t1 ->- t)   $ \f g x -> f x g 




-- | Integer arithmetic
cPlus :: Expr (Int -> Int -> Int)
cPlus = mkTerm "+" (tInt ->- tInt ->- tInt)   (+)

cTimes :: Expr (Int -> Int -> Int)
cTimes = mkTerm "*" (tInt ->- tInt ->- tInt)   (*)

cMinus :: Expr (Int -> Int -> Int)
cMinus = mkTerm "-" (tInt ->- tInt ->- tInt)   (-)

cMod :: Expr (Int -> Int -> Int)
cMod = mkTerm "-" (tInt ->- tInt ->- tInt)    (-)

cRem :: Expr (Int -> Int -> Int)
cRem = mkTerm "rem" (tInt ->- tInt ->- tInt)   mod
-- | Lists
cCons = mkTerm ":"  (t ->- TAp tList t ->- TAp tList t)    (:)
cAppend = mkTerm "++" (TAp tList t ->- TAp tList t ->- TAp tList t)   (++)
cHead = mkTerm "head" (TAp tList t ->- t)   head
cMap = mkTerm "map" ((t ->- t1) ->- TAp tList t ->- TAp tList t1)   map
cEmpty = mkTerm "[]" (TAp tList t)   []
cSingle = mkTerm "single" (t ->- TAp tList t)   $ replicate 1 
cRep = mkTerm "rep" (tInt ->- t ->- TAp tList t)   replicate 
cFoldl = mkTerm "foldl" ((t ->- t1 ->- t) ->- t ->- TAp tList t1 ->- t)    List.foldl'
cInts =  [cInt2Expr i | i <- [1..10]]
cDoubles =  [cDouble2Expr i | i <- [1..10]]

-- | A basic collection of expressions
basicExprs :: [UExpr]
basicExprs = [toUExpr cI, 
              toUExpr cS, 
              toUExpr cB, 
              toUExpr cC, 
              toUExpr cBottom,
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
