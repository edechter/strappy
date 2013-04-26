{-# Language GADTs,  ScopedTypeVariables   #-}

module Strappy.AnAlternativeApproach where

import Prelude hiding (flip)
import qualified  Data.List as List
import Unsafe.Coerce (unsafeCoerce) 
import GHC.Prim
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Trans.List

import Strappy.Type
import Strappy.Utils

import Debug.Trace


-- | Helper for turning a Haskell type to Any. 
mkAny :: a -> Any
mkAny x = unsafeCoerce x 

                    

-- | Calculate the result type of an Expr
exprType :: Expr a -> Type
exprType Term{eType=t} = t
exprType App{eLeft=el, eRight=er} = fst . runIdentity . runTI $ typeOfApp el er


----------------------------------------------------------------------
-- Library of functions

data Library = Library { probOfApp :: Double, -- ^ probability of an expansion
                         libFunctions :: [Any] }

cInt2Expr :: Int -> Expr Int
-- | Convert numbers to expressions. 
cInt2Expr i = Term (show i) tInt i 


--  Some basic library entires. 
t = mkTVar 0                  
t1 = mkTVar 1                  
t2 = mkTVar 2                  
t3 = mkTVar 3                  

cI = Term "I" (t ->- t) id
cS = Term "S" (((t2 ->- t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t)) $ \f g x -> (f x) (g x)
cB = Term "B" ((t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t) $ \f g x -> f (g x)
cC = Term "C" ((t2 ->- t1 ->- t2 ->- t) ->- t1 ->- t2 ->- t) $ \f g x -> (f x) g x
cTimes :: Expr (Int -> Int -> Int)
cTimes = Term "*" (tInt ->- tInt ->- tInt) (*)
cPlus :: Expr (Int -> Int -> Int)
cPlus = Term "+" (tInt ->- tInt ->- tInt) (+)
cCons = Term ":"  (t ->- TAp tList t ->- TAp tList t)  (:)
cAppend = Term "++" (TAp tList t ->- TAp tList t ->- TAp tList t) (++)
cHead = Term "head" (TAp tList t ->- t) head
cMap = Term "map" ((t ->- t1) ->- TAp tList t ->- TAp tList t1) map
cEmpty = Term "[]" (TAp tList t) []
cSingle = Term "single" (t ->- TAp tList t) $ \x -> [x]
cRep = Term "rep" (tInt ->- t ->- TAp tList t) $ \n x -> take n (repeat x)
cFoldl = Term "foldl" ((t ->- t1 ->- t) ->- t ->- (TAp tList t1) ->- t) $ List.foldl'
cNums =  [cInt2Expr i | i <- [1..10]]

--  A basic library

exprs :: [Any]
exprs = [mkAny cI, 
         mkAny cS, 
         mkAny cB, 
         mkAny cC, 
         mkAny cTimes, 
         mkAny cCons, 
         mkAny cEmpty,
         mkAny cAppend,
--         mkAny cHead,
         mkAny cMap,
         mkAny cFoldl,
         mkAny cSingle,
         mkAny cRep
        ] 
        ++ map mkAny cNums

library = Library 0.3 exprs


-- | Initializing a TypeInference monad with a Library. We need to
-- grab all type variables in the library and make sure that the type
-- variable counter in the state of the TypeInference monad is greater
-- that that counter.
initializeTI :: Monad m => Library -> TypeInference m ()
initializeTI Library{libFunctions=es} = do put (i + 1)
                                           return ()
    where go n (expr:rest) = let tvs = getTVars (unsafeCoerce expr :: Expr a)
                                 getTVars expr = tv . eType $ expr
                                 m = maximum $ map (readId . tyVarId) tvs 
                             in if null tvs then 0 else go (max n m) rest
          go n [] = n
          i = go 0 es
          
             
----------------------------------------------------------------------
----------------------------------------------------------------------
-- Main functions. 
sampleExpr ::
  (MonadPlus m, MonadRandom m) => Library -> Type -> m (Expr a, Int)
-- | Samples a combinator of type t from a stochastic grammar G. 
sampleExpr lib@Library{probOfApp=prApp, libFunctions=exprs} tp 
    = runTI $ do initializeTI lib
                 tp' <- freshInst tp
                 sample tp'
    where sample tp = do
            shouldExpand <- flip prApp
            case shouldExpand of
              True -> do t <- newTVar Star
                         (e_left :: Expr (b -> a))  <- unsafeCoerce $ sample (t ->- tp)
                         (e_right :: Expr b) <- unsafeCoerce $ sample (fromType (eType e_left))
                         return $ e_left <> e_right -- return application
              False -> do let cs = map fst . runTI $ filterExprsByType exprs tp
                          guard (not . null $ cs) 
                          i <- getRandomR (0, length cs - 1)
                          return $ unsafeCoerce (cs !! i) 

sampleExprs n library tp = replicate n $ sampleExpr library tp

main = sequence 
       $ do x <- sampleExprs 10 library (TAp tList tInt)
            let x' = do z <- x
                        return $ show . fst $ z
            let x'' = catch x' (const $ return "error")
            return x''



















 