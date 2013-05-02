module Strappy.Sample where

import Prelude hiding (flip)
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Random
import Control.Monad.Trans.Maybe

import Strappy.CL 
import Strappy.Type
import qualified Strappy.CombMap as CM
import Strappy.CombMap (CombMap)
import Strappy.Grammar
import Strappy.Utils 


typeOfApp :: Monad m => Comb -> Comb -> StateT Int m Type
typeOfApp c_left c_right 
    = do t <- newTVar Star 
         case mgu (cType c_left) (cType c_right ->- t) of 
           (Just sub) -> return $ toType (apply sub (cType c_left))
           Nothing -> error $ "typeOfApp: cannot unify " ++
                      show c_left ++ " with " ++ show c_right
             
sampleFromGrammar :: (MonadPlus m, MonadRandom m) =>
                     Grammar -- ^ stochastic grammar G
                  -> Type -- ^ type t
                  -> StateT Int m Comb
-- | Samples a combinator of type t from a stochastic grammar G. 
sampleFromGrammar gr tp = do shouldExpand <- flip $ expansions gr
                             case shouldExpand of
                               True -> do t <- newTVar Star
                                          c_left <- sampleFromGrammar gr (t ->- tp)
                                          c_right <- sampleFromGrammar gr (fromType (cType c_left))
                                          return $ (c_left `app'` c_right) { cReqType = tp }
                               False -> do  let  lib = library gr
                                            let cs = runStateT (filterCombinatorsByType (CM.keys lib) tp) 0
                                            let dist = [(lib CM.! c, c ) | (c, i) <- cs]
                                                z = logsumexp . map fst $ dist
                                                dist' = map (\(x, y) -> (exp (x - z), y)) $ dist 
                                            guard (not . null $ dist)
                                            sampleMultinomial dist' >>= annotateRequestedTypes tp

-- | When we draw a combinator from the library, its requested types will be too general;
-- the purpose of this procedure is to put back in the correct requested types
annotateRequestedTypes :: (MonadPlus m, MonadRandom m) =>
                          Type -> Comb -> StateT Int m Comb
annotateRequestedTypes ty comb@(CApp{lComb = l, rComb = r}) = do
  t <- newTVar Star
  l' <- annotateRequestedTypes (t ->- ty) l
  r' <- annotateRequestedTypes (fromType (cType l')) r
  return $ (l' `app'` r') { cReqType = ty }
annotateRequestedTypes ty leaf@(CLeaf{}) =
  return $ leaf { cReqType = ty }
annotateRequestedTypes _ (CInnerNode{}) =
  error "Attempt to annotate the requested type of an inner node"
annotateRequestedTypes _ hole@(CHole{}) = return hole


instance (MonadRandom m) => MonadRandom (MaybeT m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs

-- Wrapper over sampleFromGrammar that doesn't require m to instantiate MonadPlus
-- Also does not wrap the result in the type inference monad
maybeSampleFromGrammar :: MonadRandom m =>
                          Grammar -- ^ stochastic grammar G
                          -> Type -- ^ type t
                          -> m (Maybe Comb)
maybeSampleFromGrammar gr tp = do
  runMaybeT $ evalStateT (sampleFromGrammar gr tp) 0

