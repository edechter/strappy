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
             eReqType :: Maybe Type, 
             eThing :: a} -> Expr a
    App  :: {eLeft  :: (Expr (b -> a)),
             eRight :: (Expr b),
             eType  :: Type,
             eReqType :: Maybe Type, 
             eLabel :: Maybe String}         ->  Expr a 

-- | smart constructor for applications
a <> b = App a b (fst . runIdentity . runTI $ typeOfApp a b) Nothing Nothing

-- | Hide expression type in an Any type.  
data UExpr = UExpr Any

toUExpr :: Expr a -> UExpr
-- | Convert any Expr object to an object with a hidden (or Universal)
-- type.
toUExpr expr = UExpr $ unsafeCoerce expr 

fromUExpr :: UExpr -> Expr a
-- | Cvonert back from a hidden Expr object to an expression.
fromUExpr (UExpr any) = unsafeCoerce any
          
instance Show (Expr a)   where
    show Term{eName=s} = s
    show App{eLeft=el, eRight=er} = "(" ++ show el ++ " " ++  show er ++ ")"

instance Eq (Expr a) where
    e1 == e2 = show e1 == show e2

instance Ord (Expr a) where
    compare e1 e2 = compare (show e1) (show e2) 

instance Show UExpr where
    show  = show . fromUExpr  

instance Ord UExpr where
    compare ue1 ue2 = compare (show ue1) (show ue2)

instance Eq UExpr where
    ue1 == ue2 = show ue1 ==  show ue2

showableToExpr :: (Show a) => a -> Type -> Expr a
-- | Convert any Showable Haskell object into an Expr.
showableToExpr f tp = Term (show f) tp Nothing f

doubleToExpr :: Double -> Expr Double
doubleToExpr d = showableToExpr d tDouble

intToExpr :: Int -> Expr Int
intToExpr d = showableToExpr d tInt

typeOfApp :: Monad m => Expr a -> Expr b -> TypeInference m Type
typeOfApp e_left e_right 
    = do t <- newTVar Star 
         case mgu (eType e_left) (eType e_right ->- t) of 
           (Just sub) -> return $ toType (apply sub (eType e_left))
           Nothing -> error $ "typeOfApp: cannot unify " ++
                      show e_left ++ ":: " ++ show (eType e_left) 
                               ++ " with " ++ 
                      show e_right ++ ":: " ++ show (eType e_right ->- t) 

eval :: Expr a -> a
-- | Evaluates an Expression of type a into a Haskell object of that
-- corresponding type.
eval Term{eThing=f} = f
eval App{eLeft=el, eRight=er} = (eval el) (eval er)

filterExprsByType :: (Monad m) => [(UExpr, a)] -> Type -> TypeInference m [(UExpr, a)]
-- | This function takes an association list with UExpr keys and a
-- target type and returns an association list filtered to only those
-- UExprs whose types unify with the target type.
filterExprsByType ((ue, x):es) t  
    = let e = fromUExpr ue
      in do et <- freshInst . eType $ e -- use fresh type variables in
                                        -- the expression type
            case mgu et t of
              -- if the expr type unifies with the target type, apply
              -- the appropriate subsitution to the expression. 
              Just sub -> do let ueOut = toUExpr e{eType = apply sub et}
                             rest <- filterExprsByType es t
                             return $ (ueOut, x) : rest
              -- otherwise, skip
              Nothing -> filterExprsByType es t
filterExprsByType [] t = return []




----------------------------------------
-- Conversion functions ----------------
----------------------------------------


cInt2Expr :: Int -> Expr Int
-- | Convert integers to expressions. 
cInt2Expr i = Term (show i) tInt Nothing i 

cDouble2Expr :: Double -> Expr Double
-- | Convert doubles to expressions. 
cDouble2Expr i = Term (show i) tDouble Nothing i 

----------------------------------------
-- Hashable instance ------------------- 
----------------------------------------
instance Hashable (Expr a) where
    hashWithSalt a (Term name tp reqType thing) = hash a `hashWithSalt` 
                                                    hash name `hashWithSalt` hash tp 

    hashWithSalt a (App left right tp reqType name) =  hash a `hashWithSalt` hash left `hashWithSalt` 
                                               hash right `hashWithSalt` 
                                               hash tp `hashWithSalt` hash name

