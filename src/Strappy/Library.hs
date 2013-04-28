
module Library where

import Data.HashMap  (Map)
import Data.Hashable
import GHC.Prim
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.List as List

import Strappy.Type
import Strappy.Expr

type ExprMap a = Map UExpr a

instance Hashable UExpr where
    hash uexpr = hash $ fromUExpr uexpr                                     
    
-- | Helper for turning a Haskell type to Any. 
mkAny :: a -> Any
mkAny x = unsafeCoerce x 

--  Some basic library entires. 
t = mkTVar 0                  
t1 = mkTVar 1                  
t2 = mkTVar 2                  
t3 = mkTVar 3                  

-- | Basic combinators
cI = Term "I" (t ->- t) id

cS = Term "S" (((t2 ->- t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t)) $ \f g x -> (f x) (g x)

cB = Term "B" ((t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t) $ \f g x -> f (g x)

cC = Term "C" ((t2 ->- t1 ->- t2 ->- t) ->- t1 ->- t2 ->- t) $ \f g x -> (f x) g 

-- | Integer arithmetic
cPlus :: Expr (Int -> Int -> Int)
cPlus = Term "+" (tInt ->- tInt ->- tInt) (+)

cTimes :: Expr (Int -> Int -> Int)
cTimes = Term "*" (tInt ->- tInt ->- tInt) (*)

cMinus :: Expr (Int -> Int -> Int)
cMinus = Term "-" (tInt ->- tInt ->- tInt) (-)

cMod :: Expr (Int -> Int -> Int)
cMod = Term "-" (tInt ->- tInt ->- tInt) (-)

cRem :: Expr (Int -> Int -> Int)
cRem = Term "rem" (tInt ->- tInt ->- tInt) mod

-- | Lists
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

-- library = Library 0.3 exprs
