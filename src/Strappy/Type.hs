-- Type.hs
-- Eyal Dechhter
{-# Language GeneralizedNewtypeDeriving, BangPatterns,
  DeriveFunctor, ScopedTypeVariables #-}

-- | This is a different approach to type inference.
-- The type inference monad maintains a union-find data structure
-- This allows for transparent handling of the substitution

module Strappy.Type where


--  standard library imports
import qualified Data.Map as M
import Data.List (nub)
import Data.Hashable (Hashable, hash, hashWithSalt) 

import Control.Monad (foldM)
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Error
import Control.Monad.State

type Id = Int
data Type = TVar Int
          | TCon String [Type]
            deriving(Ord, Eq, Show)
infixr 6 ->-
t1 ->- t2 = TCon "->" [t1,t2]

-- | type inference monad
type TypeInference = StateT (Int, -- ^ next type var
                             M.Map Int Type) -- ^ Union-Find substitution
runTI :: Monad m => TypeInference m a -> m a
runTI = runTIVar 0

runTIVar :: Monad m => Int -> TypeInference m a -> m a
runTIVar nextTVar m =
  evalStateT m (nextTVar, M.empty)


-- Create an unbound type variable
mkTVar :: Monad m => TypeInference m Type
mkTVar = do
  (n, s) <- get
  put (n+1, s)
  return $ TVar n
  
-- Binds a type variable to a type
-- Does not check to see if the variable is not already bound
bindTVar :: Monad m => Int -> Type -> TypeInference m ()
bindTVar var ty = do
  (n, s) <- get
  put (n, M.insert var ty s)

-- Applies the current substitution to the type,
-- "chasing" bound type variables.
-- Performs path compression optimization.
-- The smaller-into-larger optimization does not apply.
applySub :: Monad m => Type -> TypeInference m Type
applySub t@(TVar v) = do
  (n, s) <- get
  case M.lookup v s of
    Just t' -> do t'' <- applySub t'
                  (n', s') <- get
                  put (n', M.insert v t'' s')
                  return t''
    Nothing -> return t
applySub (TCon k ts) =
  mapM applySub ts >>= return . TCon k

-- Unification
-- Primed unify is for types that have already had the substitution applied
unify t1 t2 = do
  t1' <- applySub t1
  t2' <- applySub t2
  unify' t1' t2'
unify' (TVar v) (TVar v') | v == v' = return ()
unify' (TVar v) t | occurs v t = lift $ fail "Occurs check"
unify' (TVar v) t = bindTVar v t
unify' t (TVar v) | occurs v t = lift $ fail "Occurs check"
unify' t (TVar v) = bindTVar v t
unify' (TCon k []) (TCon k' []) | k == k' = return ()
unify' (TCon k ts) (TCon k' ts') | k == k' = do
  zipWithM_ unify ts ts'
unify' _ _ = lift $ fail "Could not unify"

-- Occurs check: does the variable occur in the type?
occurs :: Int -> Type -> Bool
occurs v (TVar v') = v == v'
occurs v (TCon _ ts) = any (occurs v) ts


-- Checks to see if two types can unify, using current substitution
canUnifyM :: Monad m => Type -> Type -> TypeInference m Bool
canUnifyM t1 t2 = do
  state <- get
  case evalStateT (unify t1 t2) state of
    Nothing -> return False
    Just () -> return True
  
-- Non-monadic wrapper
canUnify :: Type -> Type -> Bool
canUnify t1 t2 =
  -- Ensure no clashes between type variables
  let t1' = normalizeTVars t1
      t2' = normalizeTVars t2
      t1Max = largestTVar t1'
      t2Max = largestTVar t2'
      t2'' = applyTVarSub (map (\v->(v,TVar (v+t1Max+10))) [0..t2Max]) t2'
  in case runTIVar (t1Max+t2Max+10) (canUnifyM t1' t2'') of
    Nothing -> False
    Just x -> x

-- Ground a universally quantified type by instantiating new type vars
instantiateType :: Monad m => 
                   Type -> TypeInference m Type
instantiateType ty = do
  let tvars = nub $ getTVars ty
  newTVars <- mapM (const mkTVar) tvars
  return $ applyTVarSub (zip tvars newTVars) ty
  
getTVars :: Type -> [Int]
getTVars (TVar v) = [v]
getTVars (TCon k ts) = concatMap getTVars ts

applyTVarSub :: [(Int,Type)] -> Type -> Type
applyTVarSub sub (TVar v) =
  case lookup v sub of
    Nothing -> TVar v
    Just v' -> v'
applyTVarSub sub (TCon k ts) =
  TCon k $ map (applyTVarSub sub) ts


-- Changes types like 57 -> 8 -> 57 into 0 -> 1 -> 0
normalizeTVars :: Type -> Type
normalizeTVars = snd . norm []
  where norm bs (TVar v) =
          case lookup v bs of
            Nothing -> ((v,length bs):bs, TVar (length bs))
            Just v' -> (bs, TVar v')
        norm bs t@(TCon k []) = (bs, t)
        norm bs (TCon k (t:ts)) =
          let (bs', t') = norm bs t
              (bs'', (TCon _ ts')) = norm bs' (TCon k ts)
          in (bs'', TCon k (t':ts'))

-- Gets largest type variable, returns -1 if no tvars
largestTVar :: Type -> Int
largestTVar (TVar v) = v
largestTVar (TCon _ ts) = maximum $ (-1) : map largestTVar ts

fromType :: Type -> Type
-- | Get the source type of a function type (if it is not a function
-- type then error).
fromType (TCon "->" [a,_]) = a
fromType t = error $ "Cannot apply fromType to: " ++ show t

toType :: Type -> Type
-- | Get the target type of a function type (if it is not a function
-- type then error).
toType (TCon "->" [_,a]) = a
toType t = error $ "Cannot apply toType to: " ++ show t



isTVar :: Type -> Bool
-- | Returns true if type is a singleton type variable.
isTVar (TVar _) = True
isTVar _ = False
-- TODO: Not sure what to do with this. -Kevin
{-instance Hashable Type where
    hashWithSalt a (TVar v) = hash a `hashWithSalt` hash "TyVar" `hashWithSalt`
                         hash v
    hashWithSalt a (TCon v) = hash a `hashWithSalt` hash "TyCon" `hashWithSalt`
                         hash (tyConId v) 
-}

                         

-- | Useful types. 
tGnd g = TCon g []
tChar = tGnd "Char"
tInt = tGnd "Int"
tDouble = tGnd "Double"
tBool = tGnd "Bool"
tList t = TCon "[]" [t]
tMaybe t = TCon "Maybe" [t]
tPair a b = TCon "(,)" [a,b]
tTriple a b c = TCon "(,,)" [a,b,c]
tQuad a b c d = TCon "(,,,)" [a,b,c,d]
tQuint a b c d e = TCon "(,,,,)" [a,b,c,d,e]

----------------------------------------                    
-- Typeable ----------------------------
----------------------------------------                    
class Typeable a where
	typeOf :: a ->  Type

instance Typeable Int where
	typeOf v = tInt 
instance Typeable Char where
	typeOf v = tChar
instance Typeable Double where
	typeOf v = tDouble
instance Typeable Bool where
	typeOf v = tBool
instance (Typeable a, Typeable b) =>  Typeable (a -> b)  where
	typeOf v = (typeOf (undefined :: a)) ->- (typeOf (undefined :: b)) 
instance (Typeable a) => Typeable [a] where
	typeOf v = tList (typeOf $ (undefined :: a))
instance (Typeable a, Typeable b) => Typeable (a, b) where
	typeOf v = tPair (typeOf (undefined :: a)) (typeOf (undefined :: b))

