-- Type.hs
-- Eyal Dechhter
{-# Language GeneralizedNewtypeDeriving, BangPatterns #-}

-- | This module defines a simple type system for use with the
-- combinatory logic implemented in this package. 

module Type where

-- | standard library imports
import qualified Data.Set as Set
import Data.List (find, intersect)

-- | local imports
import CLError

-- | define a type scheme
data Type = Map {fromType :: Type,
                 toType :: Type} 
          | Rtype 
          | Btype 
          | TVar Int deriving (Eq, Ord)

instance Show Type where
    show (Map t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show Rtype = "R"
    show Btype = "B" 
    show (TVar i) = "a" ++ show i

getNewTVar :: Type -> Type
getNewTVar tp = TVar (maxTVar tp + 1)
                where maxTVar (TVar j) = j
                      maxTVar (Map a b) = max (maxTVar a) (maxTVar b)
                      maxTVar _ = 0

nextTVar :: Type -> Type
nextTVar (TVar i) = (TVar $ i+1)

-- | Return type tree where each TVar is number is right ascending
-- order.
normalizeType :: Type -> Type
normalizeType t = outType
    where (outType, _) = normalize t []
          normalize s@(TVar i) sub = case findSub s sub of
                                       Nothing -> 
                                           if False -- s == minT 
                                           then (s, sub) 
                                           else (minT, (s :->: minT):sub)
                                       Just (var :->: val) -> (val, sub)
              where minT = if null sub 
                           then TVar 0 
                           else nextTVar $ foldl1 
                                          (\(TVar i) (TVar j) -> if i > j then TVar i else TVar j)
                                          [subVal t | t<-  sub]
          normalize (Map tl tr) sub = (Map outTypeLeft outTypeRight, subsLeft)
              where (outTypeRight, subsRight) = normalize tr sub
                    (outTypeLeft, subsLeft) = normalize tl $ subsRight ++ sub
          normalize t sub = (t, sub)
                                            

-- | Unification algorithm for types. Taken verbatim from Russell &
-- Norvig Chapter 9 fig. 9.1.

data Sub =  Type :->: Type deriving (Show, Eq)
subVar (v :->: _) = v
subVal (_ :->: v) = v

findSub :: Type -> [Sub] -> Maybe Sub 
{-# INLINE findSub #-}
findSub x@(TVar i) subs = find ((==x) . subVar) subs
findSub _ _ = Nothing

applySub :: Sub -> Type -> Type
applySub (var :->: val) t | var == t  = val
applySub s (Map t1 t2) = Map (applySub s t1) (applySub s t2)
applySub _ t = t

applySubs :: [Sub] -> Type -> Type
{-# INLINE applySubs #-}
applySubs ss t = if null toApply
                   then t
                   else applySubs ss (foldl (\t s -> applySub s t) t toApply)
    where toApply = filter (inType t) ss
          inType a@(TVar _) (b :->:  _) = a == b 
          inType a@(Map c d) r@(b :->: _) = a == b || inType c r 
                                           || inType d r
          inType x _ = False

-- | Unify two types and return the subsitution that unifies them.
unify :: Type -> Type -> ThrowsError [Sub]
unify t1 t2 = unify_ t1 t2 []

unify_ :: Type -> Type -> [Sub] -> ThrowsError [Sub]
unify_ x y sub | x == y       = Right sub
unify_ x@(TVar i) y sub       = unify_var x y sub
unify_ x y@(TVar i) sub       = unify_var y x sub
unify_ (Map x1 x2) (Map y1 y2) sub = unify_ x1 y1 sub 
                                     >>= \s -> unify_ (applySubs s x2) (applySubs s y2) s
                                               
unify_ x y _ = Left $ TypeError $ "Unification failed: cannot unify_ " ++ 
              show x ++ " and " ++ show y

unify_var :: Type -> Type -> [Sub] -> ThrowsError [Sub]
unify_var var@(TVar i) x sub 
    = case findSub var sub of
        (Just (_ :->: val)) -> unify_ val x sub
        Nothing -> case findSub x sub of
                     (Just (_ :->: val)) -> unify_ var val sub
                     Nothing -> if occur_check var x 
                                then Left $ TypeError $ "Unification failed: cannot unify_ " ++ 
                                     show var ++ " and " ++ show x
                                else Right $ (var :->:  x):sub

occur_check x a@(TVar i) = x == a 
occur_check x (Map e1 e2) = occur_check x e1 || occur_check x e2
occur_check x _  = False

standardize_apart x y = y `addToTVars` (maxTVar x + 1)
    where addToTVars !(TVar i) !j = (TVar (i + j))
          addToTVars !(Map e1 e2) !j = Map (addToTVars e1 j)
                                         (addToTVars e2 j)
          addToTVars x _ = x

maxTVar (TVar i) = i
maxTVar (Map e1 e2) = max (maxTVar e1) (maxTVar e2)
maxTVar _ = 0

typeCheckTypeApp :: Type -> Type -> ThrowsError Type
typeCheckTypeApp t1 t2 = do
  let beta = standardize_apart t2 (TVar 0)
      t2'@(Map _ beta') = standardize_apart t1 (Map t2 beta)
  subV <-   unify t1 t2'
  return $ applySubs subV beta'
