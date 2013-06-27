
module Strappy.Sample where

import Prelude hiding (flip)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Maybe
import Control.Monad.Random
import Control.Exception 
import Control.Arrow (second)
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe

import Strappy.Type
import Strappy.EnumBF
import Strappy.Expr
import Strappy.Library
import Strappy.Utils 
import Strappy.Config



----------------------------------------------------------------------
----------------------------------------------------------------------
-- Main functions. 
sampleExpr ::
  (MonadPlus m, MonadRandom m) => Grammar -> Type -> m Expr
-- | Samples a combinator of type t from a stochastic grammar G. 
sampleExpr gr@Grammar{grApp=p, grExprDistr=exprDistr} tp 
    = runTI $ do initializeTI exprDistr
                 tp' <- instantiateType tp
                 expr <- sample tp'
                 return expr
    where 
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
                                 eLogLikelihood = Nothing,
                                 eType = tp',
                                 eLeft = e_left,
                                 eRight = e_right }
              False -> do let cs = filter (\(e, _) -> canUnifyFast tp (eType e)) $
                                   Map.toList exprDistr
                          when (null cs) $ lift $ fail "Unable to find matching type"
                          e <- lift $ sampleMultinomial $ map (second exp) $   
                               normalizeDist cs
                          eTp <- instantiateType (eType e)
                          unify eTp tp
                          annotateRequestedM tp e

-- | Wrapper over sampleExpr that keeps trying to sample when it fails
safeSample :: MonadRandom m => Grammar -> Type -> m Expr
safeSample gr tp = do
  maybeSample <- runMaybeT $ sampleExpr gr tp
  case maybeSample of
    Nothing -> safeSample gr tp
    Just s -> return s

sampleExprs :: (MonadPlus m, MonadRandom m) =>
               Int -> Grammar -> Type -> m (ExprMap Double)
sampleExprs n library tp =
  liftM (Map.mapWithKey reweight) $ foldM accSample Map.empty [1..n]
  where accSample acc _ = do
          expr <- safeSample library tp
          return $ Map.insertWith (+) expr 1 acc
        reweight expr cnt =
          log (fromIntegral cnt) + (if usePCFGWeighting
                                    then let pcfg = fromJust $ eLogLikelihood $ pcfgLogLikelihood library expr
                                             ijcai = fromJust $ eLogLikelihood $ ijcaiLogLikelihood library expr
                                         in pcfg - ijcai
                                    else 0.0)

-- | Uses breadth-first enumeration to "sample" a grammar
-- This allows us to get many more programs
-- Each program keys its log likelihood
sampleBF :: Int -> Grammar -> Type -> ExprMap Double
sampleBF n gr tp =
  Map.fromList $ enumBF gr n tp


-- | Monadic wrapper
sampleBFM :: Monad m => Int -> Grammar -> Type -> m (ExprMap Double)
sampleBFM n gr tp = return $ sampleBF n gr tp