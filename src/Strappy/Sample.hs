module Strappy.Sample where

import Prelude hiding (flip)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Maybe
import Control.Monad.Random
import Control.Exception 
import Control.Arrow (first, second)
import qualified Data.Map as Map

import Strappy.Type
import Strappy.EnumBF
import Strappy.Expr
import Strappy.Library
import Strappy.Utils 
import Debug.Trace


----------------------------------------------------------------------
----------------------------------------------------------------------
-- Main functions. 

sampleExpr
  :: (MonadPlus m, MonadRandom m) =>
   Grammar -> Type -> m (Either TypeError Expr)
sampleExpr gr  = evalTI . sampleExprM gr 

sampleExprWithContext
  :: (MonadPlus m, MonadRandom m) =>
     Grammar -> Type -> m (Either TypeError Expr, (Int, Sub))
sampleExprWithContext gr = runTI . sampleExprM gr

sampleExprM ::
  (MonadPlus m, MonadRandom m) => Grammar -> Type -> TypeInference m Expr
-- | Samples a combinator of type t from a stochastic grammar G. 
sampleExprM gr@Grammar{grApp=p, grExprDistr=exprDistr} tp 
    = do initializeTI exprDistr
         tp' <- instantiateType tp
         sample tp'
    where 
      sample :: (MonadPlus m, MonadRandom m) => Type -> TypeInference m Expr
      sample tp = do
            shouldExpand <- lift $ flip (exp p)
            case shouldExpand of
              True -> do t <- mkTVar
                         e_left  <- sample (t ->- tp)
                         t' <- applySub t
                         e_right <- sample t'
                         tp' <- applySub tp
                         return $
                           App { eReqType = Just tp,
                                 eType = tp',
                                 eLeft = e_left,
                                 eRight = e_right, 
                                 eLogLikelihood = Nothing }
              False -> do let cs = filter (\(e, _) -> canUnifyFast tp (eType e)) $
                                   Map.toList exprDistr
                          lift $ guard (not . null $ cs)
                          e <- lift $ sampleMultinomial $ map (second exp) $   
                               normalizeDist cs
                          eTp <- instantiateType (eType e)
                          unify eTp tp
                          annotateRequestedM tp e

-- | Wrapper over sampleExpr that keeps trying to sample when it fails
safeSampleWithContext :: MonadRandom m => Grammar -> Type -> m (Either TypeError Expr, (Int, Sub)) 
safeSampleWithContext gr tp = do
  maybeSample <- runMaybeT $ sampleExprWithContext gr tp
  case maybeSample of
    Nothing -> safeSampleWithContext gr tp
    Just s -> return s

safeSample :: MonadRandom m => Grammar -> Type -> m (Either TypeError Expr)
safeSample gr tp = liftM fst $ safeSampleWithContext gr tp

sampleExprsWithContexts :: (MonadPlus m, MonadRandom m) =>
                    NextTypeVar -> Grammar -> Type -> m (ExprMap (Double, Sub))
sampleExprsWithContexts n library tp =
  liftM (Map.map (first fromIntegral)) $ foldM accSample Map.empty [1..n]
  where accSample acc _ = do
          out <- safeSampleWithContext library tp
          case out of 
          -- Insert expr into map. If expr is already a key,  increment count but keep substitution 
            (Right expr, (i, sub)) -> return $ Map.insertWith (\(a,b) (c,d) -> (a+c, b)) expr (1, sub) acc
            _ -> mzero          



-- | Uses breadth-first enumeration to "sample" a grammar
-- This allows us to get many more programs
sampleBF :: Int -> Grammar -> Type -> ExprMap Double
sampleBF n gr tp =
  Map.fromList $ map (\c -> (c, safeFromJust "BF expr has no LL" $ eLogLikelihood c)) $ enumBF gr n tp

{-putSampleExprs n library tp  
    = sequence 
      $ do x <- sampleExprs n library tp
           let x' = do z <- x
                       return $ show  z
           let x'' = catch x' (const (return "error") :: IOException -> IO String)
           return x''-}

                                          
