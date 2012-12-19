-- Type.hs
-- Eyal Dechhter
{-# Language GeneralizedNewtypeDeriving, BangPatterns,
  DeriveFunctor #-}

-- | This module defines a simple type system for use with the
-- combinatory logic implemented in this package. 

module Type where

-- | standard library imports
import qualified Data.Set as Set
import Data.List (find, intersect, union, nub, foldl')
import Control.Monad (foldM)
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Error

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

-- | type inference monad transformer

type TIError = ErrorT String

data TypeInfT m a 
    = TypeInfT { runTypeInfT :: (Subst -- ^ current set of substitutions 
                -> Int -- ^ increment integer for new types variables
                -> TIError m (Subst, Int, a))}

instance (Monad m) => Monad (TypeInfT m) where
    return x = TypeInfT (\c -> \i -> (lift . return) (c, i, x))
    m >>=  f = TypeInfT $ \s -> \i -> do
                 (s', i', y) <- runTypeInfT m s i
                 runTypeInfT (f y) s' i'

                 
instance MonadTrans TypeInfT where
    lift m = TypeInfT $ \s -> \i ->  lift $ do 
                               a <- m
                               return (s, i , a)

instance (MonadPlus m) => MonadPlus (TypeInfT m) where
    mzero = TypeInfT $ \_ -> \_ -> mzero
    m `mplus` n = TypeInfT $ \s i -> 
                  let x = do a <-  runErrorT $ runTypeInfT m s i
                             case a of 
                               Left _ -> mzero
                               Right r -> return r
                      y = do b <- runErrorT $ runTypeInfT n s i
                             case b of 
                               Left _ -> mzero
                               Right r -> return r
                  in lift $ x `mplus` y
                      
getSubst :: Monad m =>  TypeInfT m Subst
getSubst = TypeInfT (\s n -> return (s, n, s))

getVarInt :: Monad m => TypeInfT m Int
getVarInt = TypeInfT (\s n -> return (s, n, n))

putVarInt :: Monad m => Int -> TypeInfT m ()
putVarInt m  = TypeInfT (\s n -> return (s, m, ()))

putSubst :: Monad m => Subst -> TypeInfT m ()
putSubst s  = TypeInfT (\_ n -> return (s, n, ()))

extSubst :: Monad m => Subst -> TypeInfT m ()
extSubst s' = TypeInfT (\s n -> return (s'@@s, n, ()))

throwTIError :: Monad m => String -> TypeInfT m a
throwTIError e = TypeInfT (\s n -> throwError e)

                              
-- | type inference monad

type TI = TypeInfT Identity 

runTI :: TI a -> Subst -> Int -> (Subst, Int, a)
runTI ti subst i = case runIdentity $ runErrorT $ runTypeInfT ti subst i of 
                      Right x -> x
                      Left err -> error  err

runTI' :: TI a -> a
runTI' ti = let (_, _, a) = runTI ti nullSubst 0
                in a

runTISafe ti subst i = runIdentity $ runErrorT $ runTypeInfT ti subst i 

makeNewTVar :: Type -> TyVar
makeNewTVar tp = TyVar (enumId ts_next) Star
    where ts = tv tp
          ts_next = case map (\(TyVar s k) -> readId s) ts of
                      [] -> 0
                      xs -> 1 + maximum xs

-- | Type inference monad for backtracking search

type AmbTI = TypeInfT []

toList :: AmbTI a -> [a]
toList x = foldl' g [] $ runErrorT $ runTypeInfT x nullSubst 0
    where g y (Right (_, _, x)) = (x:y)
          g y (Left _) = y

takeAmbTI :: AmbTI a -> AmbTI a
takeAmbTI x = TypeInfT $ \s i -> 
              (take m) $ runErrorT $ runTypeInfT x s i


           



-- ^ Try to unify and return whether succeeded or not. 
unify' :: Monad m => Type -> Type -> TypeInfT m Bool
unify' t1 t2 = do s <- getSubst
                  case mgu (apply s t1) (apply s t2) of
                    Just u -> extSubst u >> return True
                    Nothing -> return False

unify :: Monad m => Type -> Type -> TypeInfT m ()
unify t1 t2 = do succ <-  unify' t1 t2 
                 s <- getSubst
                 case succ of 
                   True -> return ()
                   False -> throwTIError err
                       where err = "Unification error:" 
                                   ++ "Can't unify " 
                                   ++ show t1
                                   ++ " and "
                                   ++ show t2 ++ ".\n"
                                   ++ "Current subst: " 
                                   ++ show s

newTVar :: Monad m => Kind -> TypeInfT m Type 
newTVar k = TypeInfT (\s n -> 
              let v = TyVar (enumId n) k
              in return (s, n+1, TVar v))

typeCheckApp :: Monad m => Type -> Type -> TypeInfT m Type
typeCheckApp t1 t2 = do s <- getSubst
                        let t1' = apply s t1
                            t2' = apply s t2
                        tvar <- newTVar Star
                        unify t1' (t2' ->- tvar)
                        s' <- getSubst
                        return $ apply s' tvar
                         
-- ^ change all variables in a type to fresh type variables;
-- i.e. create a fresh instantiation of the type
freshInst :: Monad m => Type -> TypeInfT m Type
freshInst t = do n <- getVarInt 
                 let vi = max (ts_maxInt + 1) n
                 putVarInt vi 
                 foldM f t ts
    where ts = tv t
          ts_maxInt = case map (\(TyVar s k) -> readId s) ts of
                        [] -> 0
                        xs -> maximum xs
          f t s = do 
            (TVar u) <- newTVar (kind t)
            let t' = apply [(s, TVar u)] t
            return t'


-- | types 
tChar = TCon (TyCon "Char" Star)
tInt = TCon (TyCon "Int" Star)
tArrow = TCon (TyCon "(->)" (Kfun Star (Kfun Star Star)))

                                        
                     

