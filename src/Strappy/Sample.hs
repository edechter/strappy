
module Strappy.Sample where

import Prelude hiding (flip)
import Control.Monad.State
import Control.Monad.Random
import Control.Exception 
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
initializeTI exprDistr = do put (i + 1)
                            return ()
    where go n (uexpr:rest) = let tvs = getTVars uexpr
                                  getTVars expr = tv . eType $ (fromUExpr expr)
                                  m = maximum $ map (readId . tyVarId) tvs 
                             in if null tvs then 0 else go (max n m) rest
          go n [] = n
          i = go 0 $ map fst (Map.toList exprDistr)
          
             
----------------------------------------------------------------------
----------------------------------------------------------------------
-- Main functions. 
sampleExpr ::
  (MonadPlus m, MonadRandom m) => Grammar -> Type -> m (Expr a, Int)
-- | Samples a combinator of type t from a stochastic grammar G. 
sampleExpr gr@Grammar{grApp=p, grExprDistr=exprDistr} tp 
    = runTI $ do initializeTI exprDistr
                 tp' <- freshInst tp
                 let out = sample tp'
                 liftM fromUExpr $ out
    where 
      sample tp = do
            shouldExpand <- flip (exp p)
            case shouldExpand of
              True -> do t <- newTVar Star
                         e_left  <- sample (t ->- tp)
                         e_right <- sample (fromType (eType (fromUExpr e_left)))
                         return $ toUExpr $ (fromUExpr e_left) <> (fromUExpr e_right) 
              False -> do cs <- filterExprsByType (Map.toList exprDistr) tp
                          guard (trace (show tp) $ not . null $ cs) 
                          out <- sampleMultinomial $ map (\(e, i) -> (e, exp i)) $ 
                                             normalizeDist cs
                          (trace $ show out ) $ return out

sampleExprs n library tp = replicate n $ sampleExpr library tp

putSampleExprs n library tp  
    = sequence 
      $ do x <- sampleExprs n library tp
           let x' = do z <- x
                       return $ show . fst $ z
           let x'' = catch x' (const (return "error") :: IOException -> IO String)
           return x''

                                          
