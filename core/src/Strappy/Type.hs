-- Type.hs
-- Eyal Dechhter
{-# Language GeneralizedNewtypeDeriving, BangPatterns,
  DeriveFunctor, ScopedTypeVariables, TupleSections, GADTs #-}

-- | This is a different approach to type inference.
-- The type inference monad maintains a union-find data structure
-- This allows for transparent handling of the substitution

module Strappy.Type where


--  standard library imports
import qualified Data.Map as M
import Data.List (nub)
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Error

import Control.Monad.State
import Control.Monad.Error
import Control.DeepSeq

import Data.IORef
import System.IO.Unsafe
import Data.String (IsString)

import Strappy.Response

import Debug.Trace


-- | A data type for polymorphic types
data Type = TVar Int
          | TCon String [Type]
            deriving (Ord, Eq)

infixr 6 ->-
(->-) :: Type -> Type -> Type
t1 ->- t2 = TCon "->" [t1,t2]

-- | A context maintains a substitution and the integer corresponding
-- to the next unbound type variable.
data Context = Context {nextTVar :: !Int, subst :: M.Map Int Type} deriving (Show, Eq)

initContext :: Context
initContext = Context 0 M.empty

freshTVar :: Context -> (Type, Context)
freshTVar (Context i subst) = (TVar i, Context (i+1) subst)

-- | A type inference monad transformer. 
type TypeInference = StateT Context -- ^ Union-Find substitution

runTI :: Monad m => TypeInference m a -> m (a, Context)
runTI m = runStateT m initContext 

evalTI :: Monad m => TypeInference m a -> m a
evalTI m = evalStateT m initContext 

-- | Create a new unbound type variable
mkTVar :: Monad m => TypeInference m Type
mkTVar = do ctx <- get
            let (t, ctx') = freshTVar ctx
            put ctx'
            return $! t
  
-- | Binds a type variable to a type.
-- Does not check to see if the variable is not already bound.
bindTVar :: Monad m => Int -> Type -> TypeInference m ()
bindTVar var ty = modify $ \(Context n subst) ->
  Context n (M.insert var ty subst)

-- | Applies the current substitution to the "chasing" bound
-- type. Performs path compression optimization. The
-- smaller-into-larger optimization does not apply.
applySub :: Monad m => Type -> TypeInference m Type
applySub !t@(TVar v) = do
  Context n s <- get
  case M.lookup v s of
    Just t' -> do t'' <- applySub t'
                  Context n' s' <- get
                  bindTVar v t''
                  return t''
    Nothing -> return t
applySub !(TCon k ts) =
  mapM applySub ts >>= return . TCon k


-- TODO: This should go in testing, not here. And it should be
-- non-monadic, since it doesn't change the context.
-- -- | Debugging:
-- -- ensure that the current substitution is not circular, that is, it
-- -- does not fail the occurs check.
-- checkSubForCycles :: Monad m => TypeInference m Bool
-- checkSubForCycles =
--   let notCyclic sub hist (TVar var) =
--         if var `elem` hist then False
--         else case M.lookup var sub of
--           Just t -> notCyclic sub (var:hist) t
--           Nothing -> True
--       notCyclic sub hist (TCon _ ts) = all (notCyclic sub hist) ts
--   in do (_, s) <- get
--         return $ all (notCyclic s [] . TVar) $ M.keys s

-- | Unify two types in the type inference monad. Throw a string error on failure. 
unify :: (IsString e, MonadError e m) => Type -> Type -> TypeInference m ()
-- Note: IsString is a typeclass inhabited only by the String
-- datatype. We use it here because we are not allowed to use
-- typeclass constraints like (MonadError String m).
{-# INLINE unify #-}
unify t1 t2 = do
  t1' <- applySub t1
  t2' <- applySub t2
  unify' t1' t2'

-- TODO: we should really have a TypeInferenceError datatype.
-- | Unify two types, assuming that the current substitution has already been applied. 
unify' ::  (IsString e, MonadError e m) => Type -> Type -> TypeInference m ()
{-# INLINE unify' #-}
unify' (TVar v) (TVar v') | v == v' = return ()
unify' (TVar v) t | occurs v t = lift $ throwError "Occurs check"
unify' (TVar v) t = bindTVar v t
unify' t (TVar v) | occurs v t = lift $ throwError "Occurs check"
unify' t (TVar v) = bindTVar v t
unify' (TCon k ts) (TCon k' ts') | k == k' = do
  zipWithM_ unify ts ts'
unify' _ _ = lift $ throwError "Type mismatch: Could not unify"

-- | Occurs check. @occurs i t@ is True if @i@ occurs in @t@.
occurs :: Int -> Type -> Bool
{-# INLINE occurs #-}
occurs v (TVar v') = v == v'
occurs v (TCon _ ts) = any (occurs v) ts

-- | Return True if t1 and t2 can unify in the current context. 
canUnify :: MonadError String m => Type -> Type -> TypeInference m Bool
{-# INLINE canUnify #-}
canUnify t1 t2 = catchError (unify t1 t2 >> return True) (const $ return False)


-- Ground a universally quantified type by instantiating new type vars
instantiateType :: Monad m => Type -> TypeInference m Type
instantiateType ty = do
  let tvars = nub $ getTVars ty
  newTVars <- mapM (const mkTVar) tvars
  return $ applyTVarSub (zip tvars newTVars) ty

getTVars :: Type -> [Int]
getTVars (TVar v) = [v]
getTVars (TCon _ ts) = concatMap getTVars ts

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
tTriplet a b c = TCon "(,,)" [a,b,c]
tQuad a b c d = TCon "(,,,)" [a,b,c,d]
tQuint a b c d e = TCon "(,,,,)" [a,b,c,d,e]
tResponse = TCon "Response" []
tCase a b = TCon "Case" [a,b]
t = TVar 0                  
t1 = TVar 1               
t2 = TVar 2                  
t3 = TVar 3

-- | "Dependent Types"
tVecNil a = TCon "Vec" [tGnd "0", a]
tVecCons a = a ->- TCon "Vec" [t, a] ->- TCon "Vec" [TCon "S" [t], a]

instance Show Type where
  show (TVar id) = show id
  show (TCon "->" [l, r]) = "(" ++ show l ++ " -> " ++ show r ++ ")"
  show (TCon k []) = k
  show (TCon k ts) = "(" ++ k ++ " " ++ unwords (map show ts) ++ ")"

-- | Typeable Type Class

class Typeable a where
    typeOf :: a ->  Type

instance Typeable Int where
    typeOf _ = tInt 
instance Typeable Char where
    typeOf _ = tChar
instance Typeable Double where
    typeOf _ = tDouble
instance Typeable Bool where
    typeOf _ = tBool
instance (Typeable a, Typeable b) =>  Typeable (a -> b)  where
    typeOf _ = (typeOf (undefined :: a)) ->- (typeOf (undefined :: b)) 
instance (Typeable a) => Typeable [a] where
    typeOf _ = tList (typeOf $ (undefined :: a))
instance (Typeable a, Typeable b) => Typeable (a, b) where
    typeOf _ = tPair (typeOf (undefined :: a)) (typeOf (undefined :: b))
instance (Typeable a, Typeable b, Typeable c) => Typeable (a, b, c) where
    typeOf _ = tTriplet (typeOf (undefined :: a)) (typeOf (undefined :: b)) (typeOf (undefined :: c))
instance (Typeable a) => Typeable (Maybe a) where
    typeOf _ = tMaybe (typeOf (undefined :: a))
instance Typeable (Response) where
    typeOf _ = tResponse
-- instance (Typeable a, Typeable b) => Typeable (Case a b) where
--     typeOf _ = tCase (typeOf (undefined :: a)) (typeOf (undefined :: b))
