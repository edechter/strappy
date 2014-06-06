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
import Control.Monad (foldM)
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Error
import Control.DeepSeq

import Data.IORef
import System.IO.Unsafe
import Data.Set (Set())
import qualified Data.Set as Set
import Data.String (IsString)

import Strappy.Response
import Strappy.Case

import Debug.Trace

type Id = Int
data Type = TVar Int
          | TCon String [Type]
            deriving(Ord, Eq)
infixr 6 ->-
t1 ->- t2 = TCon "->" [t1,t2]

data Context = Context {nextTVar :: Int, subst :: M.Map Int Type} deriving (Show, Eq)
freshTVar :: Context -> (Type, Context)
freshTVar (Context i subst) = (TVar i, Context (i+1) subst)

-- | type inference monad
type TypeInference = StateT (Int, -- ^ next type var
                             M.Map Int Type) -- ^ Union-Find substitution
runTI :: Monad m => TypeInference m a -> m a
runTI = runTIVar 0

runTIVar :: Monad m => Int -> TypeInference m a -> m a
runTIVar nextTVar m =
  evalStateT m (nextTVar, M.empty)

evalTIVar nextTVar m = 
  runStateT m (nextTVar, M.empty)

evalTI = evalTIVar 0 


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
  (_, s) <- get
  case M.lookup v s of
    Just t' -> do t'' <- applySub t'
                  (n', s') <- get
                  put (n', M.insert v t'' s')
                  return t''
    Nothing -> return t
applySub (TCon k ts) =
  mapM applySub ts >>= return . TCon k


-- | Debugging: ensure that the current substitution is not circular,
-- that is, it does not fail the occurs check.
checkSubForCycles :: Monad m => TypeInference m Bool
checkSubForCycles =
  let notCyclic sub hist (TVar var) =
        if var `elem` hist then False
        else case M.lookup var sub of
          Just t -> notCyclic sub (var:hist) t
          Nothing -> True
      notCyclic sub hist (TCon _ ts) = all (notCyclic sub hist) ts
  in do (_, s) <- get
        return $ all (notCyclic s [] . TVar) $ M.keys s

-- Unification
-- Primed unify is for types that have already had the substitution applied
unify :: (IsString e, MonadError e m) => Type -> Type -> TypeInference m ()
{-# INLINE unify #-}
unify t1 t2 = do
  t1' <- applySub t1
  t2' <- applySub t2
  unify' t1' t2'

unify' ::  (IsString e, MonadError e m) => Type -> Type -> TypeInference m ()
{-# INLINE unify' #-}
unify' (TVar v) (TVar v') | v == v' = return ()
unify' (TVar v) t | occurs v t = lift $ throwError "Occurs check"
unify' (TVar v) t = bindTVar v t
unify' t (TVar v) | occurs v t = lift $ throwError "Occurs check"
unify' t (TVar v) = bindTVar v t
unify' (TCon k []) (TCon k' []) | k == k' = return ()
unify' (TCon k ts) (TCon k' ts') | k == k' = do
  zipWithM_ unify ts ts'
unify' _ _ = lift $ throwError "Could not unify"

-- Occurs check: does the variable occur in the type?
occurs :: Int -> Type -> Bool
{-# INLINE occurs #-}
occurs v (TVar v') = v == v'
occurs v (TCon _ ts) = any (occurs v) ts


-- Checks to see if two types can unify, using current substitution
canUnifyM :: Monad m => Type -> Type -> TypeInference m Bool
{-# INLINE canUnifyM #-}
canUnifyM t1 t2 = do
  state <- get
  case evalStateT (unify t1 t2) state of
    Left _ -> return False
    Right _ -> return True
  
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

canUnifyWithSomeRightTree :: Monad m 
                          => Type -- tp1
                          -> Type -- tp2
                          -> TypeInference m Bool
-- Returns true if any right subtree of tp2 can unify with tp1
canUnifyWithSomeRightTree tp1 tp2 = 
    let rightDepth tp = go 0 tp 
              where go !i tp = case tp of
                                TCon con [t1, t2] | con == "->" -> go (i+1) t2
                                _      -> i 
    in do 
     tp2' <- instantiateType tp2
     let d = rightDepth tp2'

     let loop 0 _ = return False
         loop !d tp = do
            st <- get 
            b <- canUnifyM tp tp2' 
            if b then (trace $ show tp2') return True
                 else do put st
                         s <- mkTVar 
                         loop (d - 1) (s ->- tp)
     s <- mkTVar 
     loop d (s ->- tp1)


--heuristic :: Grammmar -> Type -> Context -> Double 
---- | a lower bound on the cost of a program with the requested type
--heuristic gr tp ctx = flip evalState (nTVar, subst) $ do 



-- | Fast types that hold an implicit substitution
-- Using IORef's is dirty, and I should be using STRef's,
-- but I had difficulty appeasing the type checker using STRef's
data FType = FTVar (IORef (Maybe FType))
           | FTCon String [FType]

-- Fast, imperative procedure for checking if two types can unify
canUnifyFast :: Type -> Type -> Bool
canUnifyFast t1 t2 | structureMismatch t1 t2 = False
  where structureMismatch (TCon k xs) (TCon k' ys) = k /= k' || mismatchList xs ys
        structureMismatch _ _ = False
        mismatchList [] [] = False
        mismatchList (x:xs) (y:ys) = structureMismatch x y || mismatchList xs ys
        mismatchList _ _ = True
canUnifyFast t1 t2 = unsafePerformIO $ do
  (t1', _) <- mkFastType [] t1
  (t2', _) <- mkFastType [] t2
  uni t1' t2'
  where mkFastType dict (TVar v) =
          case lookup v dict of
            Just r -> return $ (FTVar r, dict)
            Nothing -> do r <- newIORef Nothing
                          return $ (FTVar r, (v, r) : dict)
        mkFastType dict (TCon k []) = return (FTCon k [], dict)
        mkFastType dict (TCon k ts) = do (ts', dict') <- mkFastList dict ts
                                         return (FTCon k ts', dict')
        mkFastList dict [] = return ([], dict)
        mkFastList dict (t:ts) = do
          (t', dict') <- mkFastType dict t
          (ts', dict'') <- mkFastList dict' ts
          return (t' : ts', dict'')
        
        -- Assumes both t1 and t2 have had the current substitution applied
        uni (FTVar v1) (FTVar v2) | v1 == v2 = return True
        uni (FTVar v1) t2 = if fOccurs v1 t2 then return False else (writeIORef v1 (Just t2) >> return True)
        uni t1 (FTVar v2) = if fOccurs v2 t1 then return False else (writeIORef v2 (Just t1) >> return True)
        uni (FTCon k _) (FTCon k' _) | k /= k' = return False
        uni (FTCon _ []) (FTCon _ []) = return True
        uni (FTCon _ (x:xs)) (FTCon _ (y:ys)) = do
          xy <- uni x y
          if xy then uniList xs ys else return False
        uniList [] [] = return True
        uniList (x:xs) (y:ys) = do
          x' <- traceVars x
          y' <- traceVars y
          xy <- uni x' y'
          if xy then uniList xs ys else return False
        traceVars tp@(FTVar ref) = do
          sub <- readIORef ref
          case sub of
            Nothing -> return tp
            Just tp' -> do tp'' <- traceVars tp'
                           writeIORef ref (Just tp'')
                           return tp''
        traceVars (FTCon k xs) = mapM traceVars xs >>= return . FTCon k
        fOccurs ref (FTVar ref') = ref == ref'
        fOccurs _ (FTCon _ []) = False
        fOccurs ref (FTCon _ xs) = any (fOccurs ref) xs


-- Ground a universally quantified type by instantiating new type vars
instantiateType :: Monad m => 
                   Type -> TypeInference m Type
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
tTriple a b c = TCon "(,,)" [a,b,c]
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
    typeOf _ = tTriple (typeOf (undefined :: a)) (typeOf (undefined :: b)) (typeOf (undefined :: c))
instance (Typeable a) => Typeable (Maybe a) where
    typeOf _ = tMaybe (typeOf (undefined :: a))
instance Typeable (Response) where
    typeOf _ = tResponse
instance (Typeable a, Typeable b) => Typeable (Case a b) where
    typeOf _ = tCase (typeOf (undefined :: a)) (typeOf (undefined :: b))
