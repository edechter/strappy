
module Strappy.Sample where

import Prelude hiding (flip)
import Control.Monad.State
import Control.Monad.Random
import Control.Exception 
import Control.Arrow (second)
import qualified Data.HashMap as Map

import Strappy.Type
import Strappy.Expr
import Strappy.Library
import Strappy.Utils 
import Debug.Trace


-- | Initializing a TypeInference monad with a Library. We need to
-- grab all type variables in the library and make sure that the type
-- variable counter in the state of the TypeInference monad is greater
-- that that counter.
initializeTI :: Monad m => ExprDistr -> TypeInference m ()
initializeTI exprDistr = modify $ \(_, s) -> (i, s)
    where i = maximum $
              concatMap (getTVars . eType . fromUExpr . fst) $
              Map.toList exprDistr
          
             
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
                         t' <- chaseVar t
                         e_right <- sample t'
                         tp' <- chaseVar tp
                         return $ toUExpr $
                           App { eReqType = Just tp,
                                 eType = tp',
                                 eLeft = fromUExpr e_left,
                                 eRight = fromUExpr e_right, 
                                 eLabel = Nothing }
              False -> do let cs = filter (\(e, _) -> canUnify tp (eType $ fromUExpr e)) $
                                   Map.toList exprDistr
                          lift $ guard (not . null $ cs)
                          e <- liftM fromUExpr $ lift $ sampleMultinomial $ map (second exp) $   
                                             normalizeDist cs
                          eTp <- instantiateType (eType e)
                          unify eTp tp
                          e' <- annotateRequested tp e
                          return $ toUExpr e'




sampleExprs n library tp = replicate n $ sampleExpr library tp

{-putSampleExprs n library tp  
    = sequence 
      $ do x <- sampleExprs n library tp
           let x' = do z <- x
                       return $ show  z
           let x'' = catch x' (const (return "error") :: IOException -> IO String)
           return x''-}

                                          
annotateRequested :: Monad m =>
                     Type -> -- ^ Requested type of the expression
                     Expr a -> -- ^ The expression
                     TypeInference m (Expr a) -- ^ The freshly annotated expression
annotateRequested tp (App { eLeft = l, eRight = r, eType = eTp }) = do
  t <- mkTVar
  l' <- annotateRequested (t ->- tp) l
  t' <- chaseVar t
  r' <- annotateRequested t' r
  let e = App { eLeft  = fromUExpr (toUExpr l'),
                eRight = fromUExpr (toUExpr r'),
                eType = eTp,
                eReqType = Just tp, 
                eLabel = Nothing }
  return e
annotateRequested tp e@(Term { eType = eTp }) = do
  eTp' <- instantiateType eTp
  unify tp eTp'
  return $ e { eReqType = Just tp }
