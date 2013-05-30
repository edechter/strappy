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
data Expr a where
    Term :: {eName  :: String, 
             eType  :: Type, 
             eCurrType :: Maybe Type, 
             eReqType :: Maybe Type, 
             eThing :: a} -> Expr a
    App  :: {eLeft  :: (Expr (b -> a)),
             eRight :: (Expr b),
             eType  :: Type,
             eCurrType :: Maybe Type, 
             eReqType :: Maybe Type, 
             eLabel :: Maybe String}         ->  Expr a 
             
-- | smart constructor for terms
mkTerm name tp thing = Term name tp Nothing Nothing thing

-- | smart constructor for applications
a <> b = App a b tp Nothing Nothing Nothing where tp = runIdentity . runTI $ typeOfApp a b

-- | get the current type of the expr, defaulting to base type
currentType :: Expr a -> Type
currentType expr = case eCurrType expr of
                       Nothing -> eType expr
                       Just tp -> tp

-- | Hide expression type in an Any type.  
data UExpr = UExpr Any

toUExpr :: Expr a -> UExpr
-- | Convert any Expr object to an object with a hidden (or Universal)
-- type.
toUExpr expr = UExpr $ unsafeCoerce expr 

fromUExpr :: UExpr -> Expr a
-- | Convert back from a hidden Expr object to an expression.
fromUExpr (UExpr any) = unsafeCoerce any
          
instance Show (Expr a)   where
    show Term{eName=s} = s
    show App{eLeft=el, eRight=er} = "(" ++ show el ++ " " ++  show er ++ ")"

showExprLong :: Expr a -> String
showExprLong Term{eName=n, eType=t, eCurrType=ct, eReqType=rt} = printf "%7s, type: %50s, currType: %50s, reqType: %50s" 
                                                n (show t) (show ct) (show rt)  
showExprLong App{eLeft=l, eRight=r, eType=t, eCurrType=ct,  eReqType=rt, eLabel=lb}
    = printf ("app, type: %7s, currType: %50s, reqType: %7s\n--"++showExprLong l ++ "\n--" ++ showExprLong r ++ "\n")  (show t) (show ct)  (show rt)

showUExprLong = showExprLong . fromUExpr

instance Eq (Expr a) where
    e1 == e2 = show e1 == show e2

instance Ord (Expr a) where
    compare e1 e2 = compare (show e1) (show e2) 

instance Show UExpr where
    show  = show . fromUExpr  

instance Ord UExpr where
    compare ue1 ue2 = compare (fromUExpr ue1) (fromUExpr ue2)

instance Eq UExpr where
    ue1 == ue2 = fromUExpr ue1 ==  fromUExpr ue2

showableToExpr :: (Show a) => a -> Type -> Expr a
-- | Convert any Showable Haskell object into an Expr.
showableToExpr f tp = mkTerm (show f) tp f

doubleToExpr :: Double -> Expr Double
doubleToExpr d = showableToExpr d tDouble

intToExpr :: Int -> Expr Int
intToExpr d = showableToExpr d tInt

typeOfApp :: Monad m => Expr a -> Expr b -> TypeInference m Type
typeOfApp e_left e_right 
    = do t <- mkTVar
         unify (currentType e_left) (currentType e_right ->- t)
         chaseVar t

eval :: Expr a -> a
-- | Evaluates an Expression of type a into a Haskell object of that
-- corresponding type.
eval Term{eThing=f} = f
eval App{eLeft=el, eRight=er} = (eval el) (eval er)

isLeaf :: Expr a -> Bool
-- | Returns True if the expression is a leaf, i.e., it is either a terminal or
-- it is a labelled application
isLeaf App{eLabel=Just _} = True
isLeaf _ = False

isTerm Term{} = True
isTerm _ = False
isLabeled App{eLabel=Just _} = True
isLabeled _ = False

labelExpr uexpr = toUExpr expr{eLabel=Just $ show expr} where expr=fromUExpr uexpr
unlabelExpr uexpr = toUExpr expr{eLabel=Nothing} where expr=fromUExpr uexpr                                                              
                                                              
cBottom = mkTerm "_|_" (TVar 0) (error "cBottom: this should never be called!") 

safeEval :: Expr a -> Maybe a
safeEval term@Term{eThing=f} = if (toUExpr term) == (toUExpr cBottom) 
                                then Nothing else Just f
safeEval App{eLeft = el, eRight = er} = do l <- safeEval el
                                           r <- safeEval er    
                                           return (l r) 

----------------------------------------
-- Conversion functions ----------------
----------------------------------------


cInt2Expr :: Int -> Expr Int
-- | Convert integers to expressions. 
cInt2Expr i = mkTerm (show i) tInt i 

cDouble2Expr :: Double -> Expr Double
-- | Convert doubles to expressions. 
cDouble2Expr i = mkTerm (show i) tDouble i 

----------------------------------------
-- Hashable instance ------------------- 
----------------------------------------
instance Hashable (Expr a) where
    hashWithSalt a (Term name tp _  _ thing) = hash a `hashWithSalt` hash name 
                                                  
    hashWithSalt a (App left right tp _ reqType name) =  hash a `hashWithSalt` hash left `hashWithSalt` 
                                                         hash right `hashWithSalt` hash name 

----------------------------------------
-- Expressable typeclass -------------- 
----------------------------------------
class Expressable a where
       toExpr :: a -> Expr a 

instance (Show a, Typeable a) => Expressable a where
       toExpr v = mkTerm (show v) (typeOf v) v 

