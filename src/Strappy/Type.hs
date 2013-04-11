-- Type.hs
-- Eyal Dechhter
{-# Language GeneralizedNewtypeDeriving, BangPatterns,
  DeriveFunctor #-}

-- | This module defines a Hindley-Milner type system. It is based on
-- Mark P. Jone's paper "Typing Haskell in Haskell"
-- <http://web.cecs.pdx.edu/~mpj/thih/>.

module Strappy.Type (
                    -- * Types
                    Kind(..)
                    , Type(..)
                    , TyVar(..)
                    , TyCon(..)
                    , Subst(..)
                    , TypeInference

                    -- * Functions
                    , (->-)
                    , freshInst
                    , newTVar
                    , mkTVar
                    , mgu
                    , apply
                    , merge
                    , match
                    , eqModTyVars
                    , fromType
                    , toType
                    , tv
                    , isTAp
                    , isTVar
                    , enumId
                    , readId
                    , nullSubst
                    , (-->)
                    
                           
                    -- * Combinator Types
                    , tChar 
                    , tInt 
                    , tBool 
                    , tArrow
                    , tList 
                    , tMaybe
                    , tPair 
                    , tTriple
                    , tQuad 
                    , tQuint

                    ) where

--  standard library imports
import qualified Data.Set as Set
import Data.List (intersect, union, nub, foldl')
import Data.Maybe (fromJust)
import Control.Monad (foldM)
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Error
import Control.Monad.State

type TypeInference = StateT Int

--  define a type scheme
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
-- | Function application.
--
-- >>> tInt ->- tInt
-- (Int -> Int)  
a ->- b = TAp (TAp tArrow a) b

fromType :: Type -> Type
-- | Get the source type of a function type (if it is not a function
-- type then error).
fromType (TAp (TAp tArrow a) b) = a
fromType t = error $ "Cannot apply fromType to: " ++ show t

toType :: Type -> Type
-- | Get the target type of a function type (if it is not a function
-- type then error).
toType (TAp (TAp tArrow a) b) = b

mkTVar :: Int -> Type 
-- | Convenience function that returns a singleton type consisting of
-- a type variable with specified integer id.
--
-- >>> mkTVar 5
-- v5
mkTVar i = TVar (TyVar (enumId i) Star)

isTAp :: Type -> Bool
-- | Returns true if type is a function type. 
isTAp (TAp t1 t2) = True
isTAp _ = False

isTVar :: Type -> Bool
-- | Returns true if type is a singleton type variable.
isTVar (TVar _) = True
isTVar _ = False

instance Show Type where
    show (TVar u) = show u
    show (TCon tc) = show tc
    show (TAp t1 t2) = case t1 of
                         (TAp tArrow a) -> "(" ++ show a ++ " -> " ++ show t2 ++ ")"
                         otherwise -> "(" ++ show t1 ++ " " ++ show t2 ++ ")" 


eqModTyVars :: Type -- ^ t1
            -> Type -- ^ t2
            -> Bool
-- | Type equality modulo type variables. Returns true if the mgu
-- susbtitution from t1 to t2 is just a renaming of type variables.
eqModTyVars t1 t2 = case mgu t1 t2 of
                      Just s -> all (\v -> isTVar (snd v)) s
                      Nothing -> False


-- | Substitutions
type Subst = [ (TyVar, Type) ] -- ^ list of type variable - type
                                -- associations
nullSubst :: Subst
-- | The empty substitution. 
nullSubst = []

(-->) :: TyVar -- ^ u, a type variable
      -> Type  -- ^ t, a type
      -> Subst
-- | Returns the substitution of u for t.
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

newTVar :: Monad m => Kind -> TypeInference m Type
newTVar k = do i <- get
               put (i+1)
               return $ mkTVar i

freshInst :: Monad m => Type -> TypeInference m Type
-- | Return a type where each type variable is replaced with a new,
-- unbound, type variable.
freshInst t = do let tvs = tv t 
                 ts <- mapM newTVar (map kind tvs)
                 let lk = zip (map TVar tvs) ts
                 return $ inst t lk
    where inst t@(TVar _) lk = fromJust (lookup t lk)
          inst (TAp t1 t2) lk= TAp (inst t1 lk) (inst t2 lk)
          inst t _ = t

typeLeftDepth :: Type -> Int 
-- | Returns the number of possible applications onto an expression of
-- this type.
typeLeftDepth (TAp t1 t2) = typeLeftDepth t2 + 1
typeLeftDepth _ = 1

extendTypeOnLeft :: Monad m => Type -> TypeInference m Type
-- | Returns a function type whose target is the input type and whose
-- source is a new type variable.
extendTypeOnLeft t = do tnew <- newTVar Star
                        return (tnew ->- t)

extendTypeOnLeftN :: Monad m => Type -> Int -> TypeInference m Type
-- | Apply extendTypeOnLeft N times. 
extendTypeOnLeftN t 0 = return $ t
extendTypeOnLeftN t n = do t' <- extendTypeOnLeft t
                           extendTypeOnLeftN t' (n-1)


-- | Useful types. 
tChar = TCon (TyCon "Char" Star)
tInt = TCon (TyCon "Int" Star)
tBool = TCon (TyCon "Bool" Star)
tArrow = TCon (TyCon "(->)" (Kfun Star (Kfun Star Star)))
tList = TCon $ TyCon "[]" (Kfun Star Star)
tMaybe = TCon $ TyCon "Maybe" (Kfun Star Star)                                         
tPair = TCon $ TyCon "(,)" (Kfun Star Star)
tTriple = TCon $ TyCon "(,,)" (Kfun Star Star)
tQuad = TCon $ TyCon "(,,,)" (Kfun Star Star)
tQuint = TCon $ TyCon "(,,,,)" (Kfun Star Star)

                     

