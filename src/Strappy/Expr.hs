-- Expr.hs
{-# Language GADTs,  ScopedTypeVariables   #-}

module Strappy.Expr where

import Debug.Trace
import Unsafe.Coerce (unsafeCoerce) 
import GHC.Prim
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Hashable

import Strappy.Type

-- | Main data type. Holds primitive functions (Term), their
-- application (App) and annotations.
data Expr a where
    Term :: {eName  :: String, 
             eType  :: Type, 
             eThing :: a} -> Expr a
    App  :: {eLeft  :: (Expr (b -> a)),
             eRight :: (Expr b),
             eType  :: Type,
             eLabel :: Maybe String}         ->  Expr a 

-- | smart constructor for applications
a <> b = App a b (fst . runIdentity . runTI $ typeOfApp a b)

-- | hide expression type in an Any
data UExpr = UExpr Any
toUExpr :: Expr a -> UExpr
toUExpr expr = UExpr $ unsafeCoerce expr 

fromUExpr :: UExpr -> Expr a
fromUExpr (UExpr any) = unsafeCoerce any
          
instance Show (Expr a)   where
    show Term{eName=s} = s
    show App{eLeft=el, eRight=er} = "(" ++ show el ++ " " ++  show er ++ ")"

showableToExpr :: (Show a) => a -> Type -> Expr a
showableToExpr f tp = Term (show f) tp f

doubleToExpr :: Double -> Expr Double
doubleToExpr d = showableToExpr d tDouble

intToExpr :: Int -> Expr Int
intToExpr d = showableToExpr d tInt

typeOfApp :: Monad m => Expr a -> Expr b -> TypeInference  m Type
typeOfApp e_left e_right 
    = do t <- newTVar Star 
         case mgu (eType e_left) (eType e_right ->- t) of 
           (Just sub) -> return $ toType (apply sub (eType e_left))
           Nothing -> error $ "typeOfApp: cannot unify " ++
                      show e_left ++ ":: " ++ show (eType e_left) 
                               ++ " with " ++ 
                      show e_right ++ ":: " ++ show (eType e_right ->- t) 

eval :: Expr a -> a
eval Term{eThing=f} = f
eval App{eLeft=el, eRight=er} = (eval el) (eval er)

filterExprsByType :: [Any] -> Type -> TypeInference [] Any
filterExprsByType (e:es) t  
    = do et <- freshInst (eType (unsafeCoerce e :: Expr a))
         let e' = unsafeCoerce e :: Expr a
         case mgu et t of
           Just sub -> do let eOut = unsafeCoerce e'{eType = apply sub et} :: Any
                          return eOut `mplus` rest
           Nothing -> rest
      where rest = filterExprsByType es t
filterExprsByType [] t = lift []




----------------------------------------
-- Conversion functions ----------------
----------------------------------------


cInt2Expr :: Int -> Expr Int
-- | Convert numbers to expressions. 
cInt2Expr i = Term (show i) tInt i 


----------------------------------------
-- Hashable instance ------------------- 
----------------------------------------
instance Hashable (Expr a) where
    hash (Term name tp thing) = hash name `hashWithSalt` hash tp 
    hash (App left right tp name) =  hash left `hashWithSalt` 
                                     hash right `hashWithSalt` 
                                     hash tp `hashWithSalt` 
                                     hash tp `hashWithSalt` hash name

