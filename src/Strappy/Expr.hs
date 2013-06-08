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
import Strappy.Type

-- | Main data type. Holds primitive functions (Term), their
-- application (App) and annotations.
data Expr = forall a.
            Term {eName  :: String, 
                  eType  :: Type, 
                  eReqType :: Maybe Type, 
                  eThing :: a,
                  eLogLikelihood :: Maybe Double }
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
               eReqType = Nothing, 
               eLogLikelihood = Nothing }
         where tp = runIdentity . evalTI $ typeOfApp a b

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
    compare e1 e2 = compare (show e1) (show e2) 

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

safeEval :: Expr -> Maybe a
safeEval term@Term{eThing=f} = if term == cBottom
                                then Nothing else Just $ unsafeCoerce f
safeEval App{eLeft = el, eRight = er} = do l <- safeEval el
                                           r <- safeEval er    
                                           return (l r) 

----------------------------------------
-- Conversion functions ----------------
----------------------------------------


cInt2Expr :: Int -> Expr
-- | Convert integers to expressions. 
cInt2Expr i = mkTerm (show i) tInt i 

cDouble2Expr :: Double -> Expr
-- | Convert doubles to expressions. 
cDouble2Expr i = mkTerm (show i) tDouble i 

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

