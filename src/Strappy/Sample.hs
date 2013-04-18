
module Strappy.Sample where

import Prelude hiding (flip)
import Control.Monad.State
import Control.Monad.Random

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
                                          return $ c_left `app'` c_right  
                               False -> do  let  lib = library gr
                                            let cs = runStateT (filterCombinatorsByType (CM.keys lib) tp) 0
                                            let dist = [(lib CM.! c, c ) | (c, i) <- cs]
                                                z = logsumexp . map fst $ dist
                                                dist' = map (\(x, y) -> (exp (x - z), y)) $ dist 
                                            guard (not . null $ dist)
                                            sampleMultinomial  dist'
                                 
                                                              
                                          
