{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, BangPatterns, TemplateHaskell #-}

module Strappy.IterativeDeepening where

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Trans.Either
import Control.Monad.List
import Control.Arrow
import Control.Monad.Identity
import Data.Monoid
import Data.String (IsString)
import Control.Monad.State
import Data.Maybe

import Strappy.Utils
import Strappy.Type hiding (unify, applySub, canUnifyWithSomeRightTree)
import qualified Strappy.Type as Type (unify, applySub, canUnifyWithSomeRightTree)
import Strappy.Expr
import Strappy.Library
import Strappy.Config

import Control.DeepSeq
import Control.DeepSeq.TH
import Criterion.Main

import Control.Exception
import System.IO.Unsafe
import Debug.Trace 

eps :: Double
eps = 1e-6

_SHOULD_LOG = False
logFile = "log.txt"
logToFile x a = if _SHOULD_LOG then unsafePerformIO $ do 
                                      appendFile logFile  (x ++ "\n")
                                      return a
                  else a

fromRight (Right x) = x
fromRight (Left err) = error $  "fromRight: " ++ err

isRight (Right _) = True
isRight _ = False

seedGrammar :: Grammar
seedGrammar = normalizeGrammar $ 
                    Grammar { grApp = log 0.375,
                        grExprDistr = Map.fromList
                        [ (fromRight $ annotateRequested e, 0.0) | e <- numberExprs ] }

_DEBUG = True
debug s x = if _DEBUG then trace s x else x
traceThis s x = trace (s ++ ": " ++ show x) x
debugThis s x = if _DEBUG then traceThis s x else x

infty = read ("Infinity") :: Double 

runWithContext :: Monad m => Context -> TypeInference m a -> m (a, Context)
{-# INLINE runWithContext #-}
runWithContext (Context nTVar subst) m = do 
       (a, (nTVar', subst')) <- runStateT m (nTVar, subst)
       return (a, Context nTVar' subst')

  --let (a, (nTVar', subst')) = runStateT m (nTVar, subst)
  --                                       in (a, Context nTVar subst)

unifyExpr :: Context -> Type -> Expr -> Either String Context
{-# INLINE unifyExpr #-}
unifyExpr (Context nTVar subst) t1 expr = flip evalStateT (nTVar, subst) $ do 
    t <- instantiateType (eType expr)
    Type.unify t1 t
    (n, s) <- get 
    return $ Context n s

applySub :: Context -> Type -> (Type, Context)
{-# INLINE applySub #-}
applySub (Context nTVar subst) tp = (tp', Context nTVar' subst')
  where (tp', nTVar', subst') = flip evalState (nTVar, subst) $ do 
                                      tp' <- Type.applySub tp
                                      (n, s) <- get
                                      return (tp', n, s)

canUnifyWithSomeRightTree :: Context -> Type -> Type -> Bool
canUnifyWithSomeRightTree (Context nTVar subst) t1 t2 = 
  flip evalState (nTVar, subst) $ do 
              t1' <- Type.applySub t1
              t2' <- instantiateType t2 >>= Type.applySub
              Type.canUnifyWithSomeRightTree t1' t2'


cbSearch :: Grammar 
         -> Double -- ^ Cost Bound
         -> Type  
         -> Context 
         -> ([(Expr, Double, Context)], Double)
cbSearch gr b tp ctx = if gamma > b 
                          then (leaves, min gamma cMinL)
                          else (applications, cMinA)
  where 
    gamma = negate $ grApp gr
    !(leaves, cMinL) = loop [] infty $ map (second negate) . Map.toList $ (grExprDistr gr)
          where loop out cMin [] = (out, cMin)
                loop out cMin ((e, w):es) = 
                  case unifyExpr ctx tp e of 
                        Left _ -> loop out cMin es
                        Right ctx' -> loop ((e, w, ctx'):out) 
                                           (if w > b then min w cMin else cMin) es
    (applications, cMinA) = loop leaves (gamma + k_l) lhss
          where (eta, ctx') = freshTVar ctx
                (lhss, k_l) = cbSearch gr (b - gamma) (eta ->- tp) ctx'

                loop out cMin [] = (out, cMin)
                loop out cMin ((eL, wL, ctxL):es) 
                    | gamma + wL > b = loop out cMin es
                    | otherwise      = loop out' cMin' es
                    where (rhss, k_r) = cbSearch gr (b - gamma - wL) eta ctxL
                          (out', cMin') = innerLoop out cMin rhss
                            where innerLoop out cMin [] = (out, cMin)
                                  innerLoop out cMin ((eR, wR, ctxR) : es) 
                                      | gamma + wL + wR > b = innerLoop out cMin es
                                      | otherwise  = innerLoop ((eApp, w, ctxR):out) cMin' es
                                          where eApp = (App eL eR tApp Nothing $ Just (negate (gamma + wR + wL)))
                                                w = gamma + wR + wL
                                                tApp = fst $ applySub ctxR eta
                                                cMin' = if gamma + wL + k_r > b then min cMin (gamma + wL + k_r) else cMin

lowerBound :: Grammar -> Type -> Context -> Double -> Double 
-- | Return a lower bound on the cost of a program of this type. 
-- | The lower bound is calculated recursively. Let h:Types -> Reals be the lower bound function. 
-- | If there is a primitive in the library of type t, then the weight of the least costly such primitive is returned. 
-- | Otherwise, return the cost of an application plus the sum of the lower bounds of the two application pair types. 
lowerBound gr tp ctx@(Context nTVar subst) maxB = if null es 
                                                    then minRecursiveCost 
                                                    else minimum . map (\(_, w, _) -> w) $ es
   where  es = getUnifyingExprs gr tp ctx
          minRecursiveCost = let (tp', ctx') = freshTVar ctx
                                 maxB' = maxB - negate (grApp gr)
                             in negate (grApp gr) + lowerBound gr (tp' ->- tp) ctx' maxB' + lowerBound gr tp' ctx' maxB'


cbSearchV2 :: Grammar 
         -> Double -- ^ Cost Bound
         -> Type  
         -> Context 
         -> ([(Expr, Double, Context)], Double)
cbSearchV2 gr b tp ctx = if gamma + wMin > b  
                          then (leaves, min (gamma + wMin ) cMinL)
                          else (applications, cMinA)
  where 
    wMin = minimum $ map (negate . snd) $ Map.toList $ (grExprDistr gr)
    gamma = negate $ grApp gr
    (leaves, cMinL) = loop [] infty $ map (second negate) . Map.toList $ (grExprDistr gr)
          where loop out cMin [] = (out, cMin)
                loop out cMin ((e, w):es) = 
                  case unifyExpr ctx tp e of 
                        Left _ -> loop out cMin es
                        Right ctx' -> loop ((e, w, ctx'):out) 
                                                                  (if w > b then min w cMin else cMin) es
    (applications, cMinA) = loop leaves (gamma + k_l + wMin ) lhss
          where (eta, ctx') = freshTVar ctx
                (lhss, k_l) = cbSearchV2 gr (b - gamma - wMin ) (eta ->- tp) ctx'

                loop out cMin [] = (out, cMin)
                loop out cMin ((eL, wL, ctxL):es) 
                    | gamma + wL > b = loop out cMin es
                    | otherwise      = loop out' cMin' es
                    where (rhss, k_r) = cbSearchV2 gr (b - gamma - wL) eta ctxL
                          (out', cMin') = innerLoop out cMin rhss
                            where innerLoop out cMin [] = (out, cMin)
                                  innerLoop out cMin ((eR, wR, ctxR) : es) 
                                      | gamma + wL + wR > b = innerLoop out cMin es
                                      | otherwise  = innerLoop ((eApp, w, ctxR):out) cMin' es
                                          where eApp = (App eL eR tApp Nothing $ Just (negate (gamma + wR + wL)))
                                                w = gamma + wR + wL
                                                tApp = fst $ applySub ctxR eta
                                                cMin' = if gamma + wL + k_r > b then min cMin (gamma + wL + k_r) else cMin
    --shortCircuit = not $ any (canUnifyWithSomeRightTree ctx tp) [eType e | e <- Map.keys $ grExprDistr gr]

cbSearchV3 :: Grammar 
         -> Double -- ^ Cost Bound
         -> Type  
         -> Context 
         -> ([(Expr, Double, Context)], Double)
-- | Added a the lower bound for short circuiting. Major improvement in speed. 
cbSearchV3 gr b tp ctx = if lb > b 
                           then {-# SCC lb_gt_b_V3 #-} ([], lb)
                           else if gamma + wMin > b 
                                   then (leaves, min cMinL (gamma + wMin))
                                   else (applications, cMinA)
  where 
    lb = lowerBound gr tp ctx b
    wMin = minimum $ map (negate . snd) $ Map.toList $ (grExprDistr gr)
    gamma = negate $ grApp gr
    (leaves, cMinL) = loop [] infty $ map (second negate) . Map.toList $ (grExprDistr gr)
          where loop out cMin [] = (out, cMin)
                loop out cMin ((e, w):es) = 
                  case unifyExpr ctx tp e of 
                        Left _ -> loop out cMin es
                        Right ctx' -> logToFile (show (e, w)) $ loop ((e, w, ctx'):out) cMin' es
                          where cMin' = if w > b then min w cMin else cMin
    (applications, cMinA) = loop leaves (gamma + k_l + wMin) lhss
          where (eta, ctx') = freshTVar ctx
                (lhss, k_l) = cbSearchV3 gr (b - gamma - wMin) (eta ->- tp) ctx'

                loop out cMin [] = (out, cMin)
                loop out cMin ((eL, wL, ctxL):es) 
                    | gamma + wL > b = loop out cMin es
                    | otherwise      = loop out' cMin' es
                    where (rhss, k_r) = cbSearchV3 gr (b - gamma - wL) eta ctxL
                          (out', cMin') = innerLoop out cMin rhss
                            where innerLoop out cMin [] = (out, cMin)
                                  innerLoop out cMin ((eR, wR, ctxR) : es) 
                                      | gamma + wL + wR > b = innerLoop out cMin es
                                      | otherwise  = logToFile (show (eApp, w)) $ innerLoop ((eApp, w, ctxR):out) cMin' es
                                          where eApp = (App eL eR tApp Nothing $ Just (negate (gamma + wR + wL)))
                                                w = gamma + wR + wL
                                                tApp = fst $ applySub ctxR eta
                                                cMin' = if gamma + wL + k_r > b then min cMin (gamma + wL + k_r) else cMin


getUnifyingExprs :: Grammar -> Type -> Context -> [(Expr, Double, Context)]
getUnifyingExprs gr tp ctx = [(e, negate $ w - _logZ + log (1 - exp gamma), ctx) | (e, w, ctx) <- es]
    where 
      gamma = grApp gr -- log probability of application
      es = go [] $ Map.toList (grExprDistr gr) -- list of expressions, their conditional log probability, and resulting contexts
      _logZ = logSumExpList . map (\(_, w, _) -> w ) $ es
      go !acc [] = acc
      go !acc ((e, w):es) = case unifyExpr ctx tp e of 
                                     Left _ -> go acc es
                                     Right ctx' -> go ((e, w, ctx'):acc) es
          
cbSearchV4 :: Grammar 
         -> Double -- ^ Cost Bound
         -> Type  
         -> Context 
         -> [Type] -- ^ collection of unexpanded types
         -> ([(Expr, Double, Context)], Double)
-- | Although we added a lower bound in V3, we did not update this lower bound as we progressed. 
-- | What we want is to maintain at each node, the collection of uncompleted types. 
-- | We can count lower bounds on these against the current balance. This means that 
-- | we need to pass argument types into the procedures that expand the lhss. 

cbSearchV4 gr b tp ctx ss = {-# SCC cbSearchV4 #-}  if lb > b 
                              then {-# SCC lb_gt_b_V4 #-} ([], lb)
                              else if gamma > b 
                                      then (leaves, min cMinL gamma)
                                      else (applications, cMinA)
  where 
    tp' = fst . applySub ctx $ tp
    lb = lowerBound gr tp ctx b + sum [lowerBound gr s ctx b | s <- ss] 
    gamma = negate $ grApp gr
    (leaves, cMinL) = (es, cMin')
                    where cMin' = if null ks then infty else minimum ks
                          es = getUnifyingExprs gr tp ctx
                          ks = map (\(_, w, _) -> w) . filter (\(_, w, _) -> w > b) $ es
    (applications, cMinA) = loop leaves (gamma + k_l) lhss
          where (eta, ctx') = freshTVar ctx
                (lhss, k_l) = cbSearchV4 gr (b - gamma) (eta ->- tp) ctx' (eta:ss)

                loop out cMin [] = (out, cMin)
                loop out cMin ((eL, wL, ctxL):es) 
                    | gamma + wL > b = loop out cMin es
                    | otherwise      = loop out' cMin' es
                    where (rhss, k_r) = cbSearchV4 gr (b - gamma - wL) eta ctxL ss
                          (out', cMin') = innerLoop out cMin rhss
                            where innerLoop out cMin [] = (out, cMin)
                                  innerLoop out cMin ((eR, wR, ctxR) : es) 
                                      | gamma + wL + wR > b = innerLoop out cMin es
                                      | otherwise  = logToFile (show (eApp, w)) $ innerLoop ((eApp, w, ctxR):out) cMin' es
                                          where eApp = (App eL eR tApp Nothing $ Just (negate (gamma + wR + wL)))
                                                w = gamma + wR + wL
                                                tApp = fst $ applySub ctxR eta
                                                cMin' = if gamma + wL + k_r > b then min cMin (gamma + wL + k_r) else cMin

showCbSearchResults :: ([(Expr, Double, Context)], Double) -> String
showCbSearchResults (out, cMin) = unlines $ 
  ["Max value: " ++ show cMin] ++ 
  [show e ++ ": " ++ show d | (e, d, _) <- out]

showSearchResults out = unlines $ [show e ++ ": " ++ show d | (e, d) <- out]


cbIterativeDeepening :: Grammar 
                     -> Int
                     -> Type 
                     -> [(Expr, Double)]
cbIterativeDeepening gr n tp = go 0 
  where (tp', ctx') = runIdentity . runWithContext (Context 0 Map.empty) $ instantiateType tp

        go b = let (res, b') = cbSearchV4 gr b tp' ctx' []
                   k = length res
               in if k >= n then map (\(a, b, _) -> (a, b)) res
                            else trace ("Running Iterative Deepening with bound: " ++ show b' ++ "; num progs: " ++ show k ) $ go (b' + eps)


tp = tTriple (tList tChar) (tList tChar) (tList tInt) ->- (tList tInt)
tp' = (tList tChar) ->- (tList tChar) ->- (tList tInt) ->- (tList tInt)
--tp'' = tInt ->- tInt 
main = do
  let res = cbIterativeDeepening seedGrammar 20 tp
  --let (res, mv) = cbSearchV4 seedGrammar 20 tp (Context 0  Map.empty) []
  putStr $ showSearchResults res
  --putStr $ showCbSearchResults (res, mv)
  putStr $ "Number of programs: " ++ show (length res)
  --defaultMain [
  --  --bench "cbSearchV1" $ nf (showCbSearchResults . cbSearch seedGrammar 25 tInt) (Context 0  Map.empty),
  --  --bench "cbSearchV2" $ nf (showCbSearchResults . cbSearchV2 seedGrammar 20 tInt) (Context 0  Map.empty),
  --  bench "cbSearchV3" $ nf (showCbSearchResults . cbSearchV3 seedGrammar 20 tp) (Context 0  Map.empty),
  --  bench "cbSearchV4" $ nf (showCbSearchResults . cbSearchV4 seedGrammar 20 tp (Context 0  Map.empty)) []
  --  ]

