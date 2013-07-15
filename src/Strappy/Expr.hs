-- Expr.hs
{-# Language GADTs,  ScopedTypeVariables, FlexibleInstances, UndecidableInstances   #-}

module Strappy.Expr where


import Debug.Trace
import Unsafe.Coerce (unsafeCoerce) 
import GHC.Prim
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Hashable
import Text.Printf
import Control.Exception
import Data.IORef
import System.IO.Error
import System.IO.Unsafe
import Data.Array.MArray
import Data.Array.IO
import Control.Concurrent.Timeout
import Control.Monad.Random

import Strappy.Type
import Strappy.Config
import Strappy.Utils

-- | Main data type. Holds primitive functions (Term), their
-- application (App) and annotations.
data Expr = forall a.
            Term {eName  :: String, 
                  eType  :: Type, 
                  eReqType :: Maybe Type,
                  eLogLikelihood :: Maybe Double,
                  eThing :: a }
          | App {eLeft  :: Expr,
                 eRight :: Expr,
                 eType  :: Type,
                 eReqType :: Maybe Type, 
                 eLogLikelihood :: Maybe Double }
             
-- | smart constructor for terms
mkTerm name tp thing = Term { eName = name,
                              eType = tp, 
                              eReqType = Nothing,
                              eLogLikelihood = Nothing,
                              eThing = thing }

-- | smart constructor for applications
a <> b = App { eLeft = a, 
               eRight = b, 
               eType = tp, 
               eLogLikelihood = Nothing,
               eReqType = Nothing }
         where tp = runIdentity . runTI $ typeOfApp a b

instance Show Expr where
    show Term{eName=s} = s
    show App{eLeft=el, eRight=er} = "(" ++ show el ++ " " ++  show er ++ ")"

showExprLong :: Expr -> String
showExprLong Term{eName=n, eType=t, eReqType=rt} = printf "%7s, type: %50s, reqType: %50s" 
                                                n (show t) (show rt)  
showExprLong App{eLeft=l, eRight=r, eType=t, eReqType=rt }
    = printf ("app, type: %7s, reqType: %7s\n--"++showExprLong l ++ "\n--" ++ showExprLong r ++ "\n")  (show t)  (show rt)

instance Eq Expr where
    e1 == e2 = show e1 == show e2

instance Ord Expr where
    compare (Term {eName = n}) (Term {eName = n'}) = compare n n' 
    compare (App {}) (Term {}) = LT
    compare (Term {}) (App {}) = GT
    compare (App { eLeft = l, eRight = r }) (App { eLeft = l', eRight = r' }) =
      case compare l l' of
        EQ -> compare r r'
        cmp -> cmp

showableToExpr :: (Show a) => a -> Type -> Expr
-- | Convert any Showable Haskell object into an Expr.
showableToExpr f tp = mkTerm (show f) tp f

doubleToExpr :: Double -> Expr
doubleToExpr d = showableToExpr d tDouble

intToExpr :: Int -> Expr
intToExpr d = showableToExpr d tInt

typeOfApp :: Monad m => Expr -> Expr -> TypeInference m Type
typeOfApp e_left e_right 
    = do t <- mkTVar
         unify (eType e_left) (eType e_right ->- t)
         applySub t

eval :: Expr -> a
-- | Evaluates an Expression of type a into a Haskell object of that
-- corresponding type.
eval Term{eThing=f} = unsafeCoerce f
eval App{eLeft=el, eRight=er} = (eval el) (eval er)


isTerm :: Expr -> Bool
isTerm Term{} = True
isTerm _ = False

cBottom = mkTerm "_|_" (TVar 0) (error "cBottom: this should never be called!") 

timeLimitedEval :: Show a => Expr -> Maybe a
timeLimitedEval expr = unsafePerformIO $
                       flip Control.Exception.catch (\e -> return $ const Nothing (e :: SomeException)) $ do
                       res <- timeout maxEvalTime $ do
                         -- Hack to force Haskell to evaluate the expression:
                         -- Convert the (eval expr) in to a string,
                         -- then force each character of the string by putting it in to an unboxed array
                         let val = eval expr
                         let strVal = show val
                         a <- ((newListArray (1::Int,length strVal) strVal) :: IO (IOUArray Int Char))
                         return val
                       return res


-- | Runs type inference on the given program, returning its type
doTypeInference :: Expr -> Type
doTypeInference expr = runIdentity $ runTI $ infer expr
  where infer (Term { eType = tp }) = instantiateType tp
        infer (App { eLeft = l, eRight = r }) = do
          alpha <- mkTVar
          beta <- mkTVar
          lTp <- infer l
          unify lTp (alpha ->- beta)
          rTp <- infer r
          unify rTp alpha
          applySub beta


-- | Procedures for managing holes:
-- | Returns the number of holes in an expression
countHoles :: Expr -> Int
countHoles (App { eLeft = l, eRight = r }) = countHoles l + countHoles r
countHoles (Term { eName = "H" }) = 1
countHoles (Term {}) = 0

-- | Samples values for the holes, and computes the log expected likelihood
sampleHoles :: MonadRandom m =>
               (Expr -> Double) -> Int -> Expr -> m Double
sampleHoles ll samples e = do
  lls <- replicateM samples (sample' e >>= \s -> return (ll s))
  let retval = logSumExpList lls - log (fromIntegral samples)
  return retval
  where sample' (Term { eName = "H" }) = do
          h <- sampleMultinomial [(0.0, 0.1::Double), (0.1, 0.1), (0.2, 0.1),
                                  (0.3, 0.1), (0.4, 0.1), (0.5, 0.1),
                                  (0.6, 0.1), (0.7, 0.1), (0.8, 0.1),
                                  (0.9, 0.1)]
          return $ cDouble2Expr h
        sample' e@(App { eLeft = l, eRight = r}) = do
          l' <- sample' l
          r' <- sample' r
          return $ e { eLeft = l', eRight = r' }
        sample' e = return e

----------------------------------------
-- Conversion functions ----------------
----------------------------------------


cInt2Expr :: Int -> Expr
-- | Convert integers to expressions. 
cInt2Expr i = mkTerm (show i) tInt i 

cDouble2Expr :: Double -> Expr
-- | Convert doubles to expressions. 
cDouble2Expr i = mkTerm (show i) tDouble i 

cBool2Expr :: Bool -> Expr
cBool2Expr b = mkTerm (show b) tBool b

----------------------------------------
-- Hashable instance ------------------- 
----------------------------------------
instance Hashable Expr where
    hashWithSalt a (Term { eName = name }) = hash a `hashWithSalt` hash name 
                                                  
    hashWithSalt a (App { eLeft = left, eRight = right }) = 
      hash a `hashWithSalt` hash left `hashWithSalt` hash right

----------------------------------------
-- Expressable typeclass -------------- 
----------------------------------------
class Expressable a where
       toExpr :: a -> Expr

instance (Show a, Typeable a) => Expressable a where
       toExpr v = mkTerm (show v) (typeOf v) v 
