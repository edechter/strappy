
module Strappy.Sample where

import Prelude hiding (flip)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Maybe
import Control.Monad.Random
import Control.Exception 
import Control.Arrow (second)
import qualified Data.HashMap as Map

import Strappy.Type
import Strappy.EnumBF
import Strappy.Expr
import Strappy.Library
import Strappy.Utils 
import Debug.Trace


----------------------------------------------------------------------
----------------------------------------------------------------------
-- Main functions. 
sampleExpr ::
  (MonadPlus m, MonadRandom m) => Grammar -> Type -> m (Expr a)
-- | Samples a combinator of type t from a stochastic grammar G. 
sampleExpr gr@Grammar{grApp=p, grExprDistr=exprDistr} tp 
    = runTI $ do initializeTI exprDistr
                 tp' <- instantiateType tp
                 expr <- sample tp'
                 return $ fromUExpr expr
    where 
      sample tp = do
            shouldExpand <- lift $ flip (exp p)
            case shouldExpand of
              True -> do t <- mkTVar
                         e_left  <- sample (t ->- tp)
                         t' <- applySub t
                         e_right <- sample t'
                         tp' <- applySub tp
                         return $ toUExpr $
                           App { eReqType = Just tp,
                                 eType = tp',
                                 eLeft = fromUExpr e_left,
                                 eRight = fromUExpr e_right, 
                                 eLabel = Nothing, 
                                 eLogLikelihood = Nothing }
              False -> do let cs = filter (\(e, _) -> canUnify tp (eType $ fromUExpr e)) $
                                   Map.toList exprDistr
                          lift $ guard (not . null $ cs)
                          e <- liftM fromUExpr $ lift $ sampleMultinomial $ map (second exp) $   
                                             normalizeDist cs
                          eTp <- instantiateType (eType e)
                          unify eTp tp
                          e' <- annotateRequestedM tp e
                          return $ toUExpr e'

-- | Wrapper over sampleExpr that keeps trying to sample when it fails
safeSample :: MonadRandom m => Grammar -> Type -> m (Expr a)
safeSample gr tp = do
  maybeSample <- runMaybeT $ sampleExpr gr tp
  case maybeSample of
    Nothing -> safeSample gr tp
    Just s -> return s

sampleExprs :: (MonadPlus m, MonadRandom m) =>
               Int -> Grammar -> Type -> m (ExprMap Double)
sampleExprs n library tp =
  liftM (Map.map fromIntegral) $ foldM accSample Map.empty [1..n]
  where accSample acc _ = do
          expr <- safeSample library tp
          return $ Map.insertWith (+) (toUExpr expr) 1 acc

-- | Uses breadth-first enumeration to "sample" a grammar
-- This allows us to get many more programs
sampleBF :: Int -> Grammar -> Type -> ExprMap Double
sampleBF n gr tp =
  Map.fromList $ map (\CombBase{comb=c} -> (toUExpr c, 1.0)) $ enumBF gr n tp

{-putSampleExprs n library tp  
    = sequence 
      $ do x <- sampleExprs n library tp
           let x' = do z <- x
                       return $ show  z
           let x'' = catch x' (const (return "error") :: IOException -> IO String)
           return x''-}

                                          
-- | Annotates the requested types
-- Takes as input the top-level type request
annotateRequestedM :: Monad m =>
                     Type -> -- ^ Requested type of the expression
                     Expr a -> -- ^ The expression
                     TypeInference m (Expr a) -- ^ The freshly annotated expression
annotateRequestedM tp (App { eLeft = l, eRight = r }) = do
  t <- mkTVar
  l' <- annotateRequestedM (t ->- tp) l
  t' <- applySub t
  r' <- annotateRequestedM t' r
  tp' <- applySub tp
  let e = App { eLeft    = fromUExpr (toUExpr l'),
                eRight   = fromUExpr (toUExpr r'),
                eType    = tp',
                eReqType = Just tp, 
                eLabel   = Nothing, 
                eLogLikelihood = Nothing }
  return e
annotateRequestedM tp e@(Term { eType = eTp }) = do
  eTp' <- instantiateType eTp
  unify tp eTp'
  return $ e { eReqType = Just tp }

-- | Non-monadic wrapper
-- Presumes no constraint on top-level type
annotateRequested :: UExpr -> UExpr
annotateRequested expr = runIdentity $ runTI $ do
  tp <- mkTVar
  liftM toUExpr $ annotateRequestedM tp (fromUExpr expr)
