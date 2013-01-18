-- Type.hs
-- Eyal Dechhter
{-# Language GeneralizedNewtypeDeriving, BangPatterns,
  DeriveFunctor #-}

-- | This module defines a simple type system for use with the
-- combinatory logic implemented in this package. 

module Type where

-- | standard library imports
import qualified Data.Set as Set
import Data.List (intersect, union, nub, foldl')
import Data.Maybe (fromJust)
import Control.Monad (foldM)
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Error
import Control.Monad.State

import Data.MemoTrie
import Debug.Trace

-- | define a type scheme
type Id = String
enumId :: Int -> Id
enumId n = "v" ++ show n
readId :: String -> Int
readId ('v':xs) = read xs

data Kind = Star | Kfun Kind Kind
          deriving (Eq, Ord)

instance Show Kind where
    show Star = "*"
    show (Kfun k k') = "(" ++ show k ++ " -> " ++ show k' ++ ")" 

data Type = TVar TyVar
          | TCon TyCon
          | TAp Type Type
          deriving (Eq, Ord)

data TyVar = TyVar Id Kind
             deriving (Eq, Ord)

instance Show TyVar where
    show (TyVar i k) = i

data TyCon = TyCon Id Kind 
           deriving (Eq, Ord)
instance Show TyCon where
    show (TyCon i k) = i


class HasKind t where
    kind :: t -> Kind
instance HasKind TyVar where
    kind (TyVar v k) = k
instance HasKind TyCon where
    kind (TyCon v k ) = k
instance HasKind Type where
    kind (TVar u) = kind u
    kind (TCon tc) = kind tc
    kind (TAp t _) = case (kind t) of 
                       (Kfun _ k) -> k

infixr 4 ->-
(->-) :: Type -> Type -> Type
a ->- b = TAp (TAp tArrow a) b

fromType :: Type -> Type
fromType (TAp (TAp tArrow a) b) = a

toType :: Type -> Type
toType (TAp (TAp tArrow a) b) = b

mkTVar :: Int -> Type 
mkTVar i = TVar (TyVar (enumId i) Star)



isTVar :: Type -> Bool
isTVar (TVar _) = True
isTVar _ = False

instance Show Type where
    show (TVar u) = show u
    show (TCon tc) = show tc
    show (TAp t1 t2) = case t1 of
                         (TAp tArrow a) -> "(" ++ show a ++ " -> " ++ show t2 ++ ")"
                         otherwise -> "(" ++ show t1 ++ " " ++ show t2 ++ ")" 

--  type equality modulo type variables
eqModTyVars :: Type -> Type -> Bool
eqModTyVars t1 t2 = case mgu t1 t2 of
                      Just s -> all (\v -> isTVar (snd v)) s
                      Nothing -> False


-- | Substitutions
type Subst = [ (TyVar, Type) ] -- ^ list of type variable - type
                                -- associations
nullSubst :: Subst
nullSubst = []

(-->) :: TyVar -> Type -> Subst
u --> t = [(u, t)]

class Types t where
    apply :: Subst -> t -> t
    tv ::  t -> [TyVar]

instance Types Type where
    --  apply a substitution to a type
    {-# INLINE apply #-}
    apply s (TVar u) = case lookup u s of
                         Just t -> t
                         Nothing -> TVar u
    apply s (TAp l r) = TAp (apply s l) (apply s r)
    apply s t = t

    -- extract all type variables from a type
    tv (TVar u) = [u]
    tv (TAp l r) = tv l `union` tv r
    tv _ = []

instance Types a => Types [a] where
    apply s = map (apply s)
    tv  = nub . concat . map tv

-- combine two substitutions into one
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
{-# INLINE (@@) #-}
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

-- merge two substitutions ensuring that both agree on all variables
merge :: Subst -> Subst -> Maybe Subst
merge s1 s2 = if agree then Just s else Nothing
    where dom s = map fst s
          s = s1 ++ s2
          agree = all ( \v -> apply s1 (TVar v) == 
                             apply s2 (TVar v))
                  (dom s1 `intersect` dom s2)

-- most general unifier 
mgu :: Type -> Type -> Maybe Subst
mgu (TAp l r) (TAp l' r') = do s1 <- mgu l l'
                               s2 <- mgu (apply s1 r)
                                     (apply s1 r')
                               Just (s2 @@ s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = Just nullSubst
mgu t s = Nothing

-- bind variable to type, performing occurs check
varBind :: TyVar -> Type -> Maybe Subst
varBind u t | t == TVar u  = Just nullSubst
            | u `elem` tv t = Nothing
            | kind u == kind t = Just $ u --> t
            | otherwise = Nothing

-- matching : given types t1 t2, find substitution s s.t. apply s t1 =
-- t2
match' :: Type -> Type -> Type -> Type -> Maybe Subst
match' l r l' r' = do s1 <- match l l'
                      s2 <- match r r'
                      merge s1 s2

match :: Type -> Type -> Maybe Subst
match (TAp l r) (TAp l' r') = do sl <- match l l'
                                 sr <- match r r'
                                 merge sl sr
match (TVar u) t 
    | kind u  == kind t   = Just $ u --> t
match (TCon tc1) (TCon tc2) | tc1 == tc2 = Just nullSubst
match t1 t2 = Nothing

makeNewTVar :: [Type] -> TyVar
makeNewTVar tps = TyVar (enumId ts_next) Star
    where ts = concat (map tv tps)
          ts_next = case map (\(TyVar s k) -> readId s) ts of
                      [] -> 0
                      xs -> 1 + maximum xs

newTVar :: Monad m => Kind -> StateT Int m Type
newTVar k = do i <- get
               put (i+1)
               return $ mkTVar i

freshInst :: Monad m => Type -> StateT Int m Type
freshInst t = do let tvs = tv t 
                 ts <- mapM newTVar (map kind tvs)
                 let lk = zip (map TVar tvs) ts
                 return $ inst t lk
    where inst t@(TVar _) lk = fromJust (lookup t lk)
          inst (TAp t1 t2) lk= TAp (inst t1 lk) (inst t2 lk)
          inst t _ = t

-- | types 
tChar = TCon (TyCon "Char" Star)
tInt = TCon (TyCon "Int" Star)
tBool = TCon (TyCon "Bool" Star)
tArrow = TCon (TyCon "(->)" (Kfun Star (Kfun Star Star)))
tList = TCon $ TyCon "[]" (Kfun Star Star)
tMaybe = TCon $ TyCon "Maybe" (Kfun Star Star)                                         
tPair = TCon $ TyCon "(,)" (Kfun Star Star)
                     

