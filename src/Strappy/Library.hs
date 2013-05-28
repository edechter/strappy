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
-- | TODO: the use of isLeaf here is incorrect, because it only looks at the label of the expression, which refers to a previous grammar.
exprLogLikelihood gr expr = let e = toUExpr expr in
    -- | Is this expr a leaf?
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
    where isTerm Term{} = True
          isTerm _ = False
          isLabeled App{eLabel=Just _} = True
          isLabeled _ = False

data Counts = Counts {appCounts :: Double,
                        termCounts :: Double,
                        useCounts :: ExprMap Double,
                        possibleUseCounts :: ExprMap Double} deriving Show
estimateGrammar :: Grammar 
                -> Double -- pseudocounts
                -> [(UExpr, Double)] -- weighted observations 
                -> Grammar
estimateGrammar Grammar{grExprDistr=distr, grApp = app} pseudocounts obs 
    = Grammar{grApp=appLogProb, grExprDistr=distr'}
    where es = Map.toList distr -- [(UExpr, Double)]
          -- Accumulator function that takes a current records of counts and an expression and undates the counts.
          go :: Counts -> Expr a -> Counts
          -- If expression is a terminal. 
          go (Counts ac tc uc pc) expr@Term{eReqType=(Just tp)} =
            let tc' = tc + pseudocounts
                uc' = Map.adjust (+ pseudocounts) (toUExpr expr) uc
                otherTerms = map fst . fst . runIdentity . runTI $ filterExprsByType es tp
                pc' = List.foldl' (Prelude.flip $ Map.adjust (+pseudocounts)) pc otherTerms 
            in Counts ac tc' uc' pc'
          go counts@(Counts ac tc uc pc) expr@App{eRight=r, eLeft=l} = let countsLeft = go counts l
                                                                           countsRight = go countsLeft r
                                                                        in countsRight{appCounts= appCounts countsRight + pseudocounts}
          empty = Map.map (const 0) distr
          counts = List.foldl' go (Counts 0 0 empty empty) (map (fromUExpr . fst) obs)
          appLogProb = (trace $ show counts) $ log (appCounts counts) - log (termCounts counts + appCounts counts)
          distr' = Map.unionWith (\a a' -> log a - log (a + a')) (useCounts counts) (possibleUseCounts counts)
          
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
cI = Term "I" (t ->- t) Nothing id

cS = Term "S" ((t2 ->- t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t) Nothing $ \f g x -> f x (g x)

cB = Term "B" ((t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t) Nothing $ \f g x -> f (g x)

cC = Term "C" ((t1 ->- t2 ->- t) ->- t2 ->- t1 ->- t) Nothing $ \f g x -> f x g 




-- | Integer arithmetic
cPlus :: Expr (Int -> Int -> Int)
cPlus = Term "+" (tInt ->- tInt ->- tInt) Nothing (+)

cTimes :: Expr (Int -> Int -> Int)
cTimes = Term "*" (tInt ->- tInt ->- tInt) Nothing (*)

cMinus :: Expr (Int -> Int -> Int)
cMinus = Term "-" (tInt ->- tInt ->- tInt) Nothing (-)

cMod :: Expr (Int -> Int -> Int)
cMod = Term "-" (tInt ->- tInt ->- tInt) Nothing  (-)

cRem :: Expr (Int -> Int -> Int)
cRem = Term "rem" (tInt ->- tInt ->- tInt) Nothing mod
-- | Lists
cCons = Term ":"  (t ->- TAp tList t ->- TAp tList t)  Nothing (:)
cAppend = Term "++" (TAp tList t ->- TAp tList t ->- TAp tList t) Nothing (++)
cHead = Term "head" (TAp tList t ->- t) Nothing head
cMap = Term "map" ((t ->- t1) ->- TAp tList t ->- TAp tList t1) Nothing map
cEmpty = Term "[]" (TAp tList t) Nothing []
cSingle = Term "single" (t ->- TAp tList t) Nothing $ replicate 1 
cRep = Term "rep" (tInt ->- t ->- TAp tList t) Nothing replicate 
cFoldl = Term "foldl" ((t ->- t1 ->- t) ->- t ->- TAp tList t1 ->- t) Nothing  List.foldl'
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
