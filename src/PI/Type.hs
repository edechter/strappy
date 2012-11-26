-- Type.hs
-- Eyal Dechhter
{-# Language GeneralizedNewtypeDeriving, BangPatterns #-}

-- | This module defines a simple type system for use with the
-- combinatory logic implemented in this package. 

module Type where

-- | standard library imports
import qualified Data.Set as Set
import Data.List (find, intersect, union)
import Control.Monad (foldM)

-- -- | local imports
import CLError

-- | define a type scheme

data TyVar = TyVar Int deriving (Eq, Ord, Show)
mkTVar :: Int -> Type
mkTVar i = TVar (TyVar i)

data Type = Map {fromType :: Type,
                 toType :: Type} 
          | Prod {projl :: Type, 
                  projr :: Type}
          | Sum {outl :: Type,
                 outr :: Type} 
          | TyIntList 
          | Rtype 
          | Btype 
          | TVar TyVar deriving (Eq, Ord)

instance Show Type where
    show (Map t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show (Prod t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
    show (Sum t1 t2) = "(" ++ show t1 ++ " | " ++ show t2 ++ ")"
    show (TyIntList) = "[R]"
    show Rtype = "R"
    show Btype = "B" 
    show (TVar (TyVar i)) = "a" ++ show i

-- | Substitutions
type Subst = [ (TyVar, Type) ] -- ^ list of type variable - type
                                -- associations
nullSubst :: Subst
nullSubst = []

(-->) :: TyVar -> Type -> Subst
u --> t = [(u, t)]

--  apply a substitution to a type
apply :: Subst -> Type -> Type
apply s (TVar u) = case lookup u s of
                     Just t -> t
                     Nothing -> TVar u
apply s (Map l r) = Map (apply s l) (apply s r)
apply s (Prod l r) = Prod (apply s l) (apply s r)
apply s (Sum l r) = Sum (apply s l) (apply s r)
apply s t = t

-- extract all type variables from a type
tv (TVar u) = [u]
tv (Map l r) = tv l `union` tv r
tv (Prod l r) = tv l `union` tv r
tv (Sum l r) = tv l `union` tv r
tv _ = []

-- combine two substitutions into one
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
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
mgu (Map l r) (Map l' r') = do s1 <- mgu l l'
                               s2 <- mgu (apply s1 r)
                                     (apply s1 r')
                               Just (s2 @@ s1)
mgu (Prod l r) (Prod l' r') = do s1 <- mgu l l'
                                 s2 <- mgu (apply s1 r)
                                       (apply s1 r')
                                 Just (s2 @@ s1)
mgu (Sum l r) (Sum l' r') = do s1 <- mgu l l'
                               s2 <- mgu (apply s1 r)
                                     (apply s1 r')
                               Just (s2 @@ s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu t s | t == s = Just nullSubst
        | otherwise = Nothing

-- bind variable to type, performing occurs check
varBind :: TyVar -> Type -> Maybe Subst
varBind u t | t == TVar u  = Just nullSubst
            | u `elem` tv t = Nothing
            | otherwise = Just $ u --> t

-- matching : given types t1 t2, find substitution s s.t. apply s t1 =
-- t2
match' :: Type -> Type -> Type -> Type -> Maybe Subst
match' l r l' r' = do s1 <- match l l'
                      s2 <- match r r'
                      merge s1 s2

match :: Type -> Type -> Maybe Subst
match (Map l r) (Map l' r') = match' l r l' r'
match (Prod l r) (Prod l' r') = match' l r l' r'
match (Sum l r) (Sum l' r') = match' l r l' r'
match (TVar u) t = Just $ u --> t
match _ _ = Nothing
                              

-- | type inference monad
data TI a = TI (Subst -- ^ current set of substitutions 
                    -> Int -- ^ increment integer for new types variables
                    -> ThrowsError (Subst, Int, a))

instance Monad TI where
    return x = TI (\c -> \i -> Right (c, i, x))
    (TI c) >>= f = (TI c')
        where c' s n = do (s', m, x) <- c s n 
                          let TI fx = f x
                          fx s' m

runTI :: TI a -> a
runTI (TI c) = case c nullSubst 0 of 
                      Right (s, n, result) -> result
                      Left err -> error $ showError err

hasTIError :: TI a -> Bool
hasTIError (TI c) = case c nullSubst 0 of
                      Right _ -> True
                      Left _ -> False

getSubst :: TI Subst
getSubst = TI (\s n -> Right (s, n, s))

extSubst :: Subst -> TI ()
extSubst s' = TI (\s n -> Right (s'@@s, n, ()))

throwTIError :: CLError -> TI a
throwTIError e = TI (\s n -> Left e)


-- ^ Try to unify and return whether succeeded or not. 
unify' :: Type -> Type -> TI Bool
unify' t1 t2 = do s <- getSubst
                  case mgu (apply s t1) (apply s t2) of
                    Just u -> extSubst u >> return True
                    Nothing -> return False

unify :: Type -> Type -> TI ()
unify t1 t2 = do succ <-  unify' t1 t2 
                 case succ of 
                   True -> return ()
                   False -> throwTIError $ TypeError "Unification error."
                     


newTVar :: TI Type 
newTVar = TI (\s n -> 
              let v = TyVar n 
              in Right (s, n+1, TVar v))

typeCheckApp :: Type -> Type -> TI Type
typeCheckApp t1 t2 = do s <- getSubst
                        let t1' = apply s t1
                            t2' = apply s t2
                        tvar <- newTVar
                        unify t1' (Map t2' tvar)
                        s' <- getSubst
                        return $ apply s' tvar
                         
-- ^ change all variables in a type to fresh type variables;
-- i.e. create a fresh instantiation of the type
freshInst :: Type -> TI Type
freshInst t = foldM f t ts
    where ts = tv t
          f t s = do (TVar u) <- newTVar
                     let t' = apply [(s, TVar u)] t
                     return t'
                
                  
                        
                     

