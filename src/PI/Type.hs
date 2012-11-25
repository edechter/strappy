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
                
                  
                        
                     

-- getNewTVar :: Type -> Type
-- getNewTVar tp = TVar (maxTVar tp + 1)
--                 where maxTVar (TVar j) = j
--                       maxTVar (Map a b) = max (maxTVar a) (maxTVar b)
--                       maxTVar _ = 0

-- nextTVar :: Type -> Type
-- nextTVar (TVar i) = (TVar $ i+1)

-- -- | Return type tree where each TVar is number is right ascending
-- -- order.
-- normalizeType :: Type -> Type
-- normalizeType t = outType
--     where (outType, _) = normalize t []
--           normalize s@(TVar i) sub = case findSub s sub of
--                                        Nothing -> 
--                                            if False -- s == minT 
--                                            then (s, sub) 
--                                            else (minT, (s :->: minT):sub)
--                                        Just (var :->: val) -> (val, sub)
--               where minT = if null sub 
--                            then TVar 0 
--                            else nextTVar $ foldl1 
--                                           (\(TVar i) (TVar j) -> if i > j then TVar i else TVar j)
--                                           [subVal t | t<-  sub]
--           normalize (Map tl tr) sub = (Map outTypeLeft outTypeRight, subsLeft)
--               where (outTypeRight, subsRight) = normalize tr sub
--                     (outTypeLeft, subsLeft) = normalize tl $ subsRight ++ sub
--           normalize t sub = (t, sub)
                                            

-- -- | Unification algorithm for types. Taken verbatim from Russell &
-- -- Norvig Chapter 9 fig. 9.1.

-- data Sub =  Type :->: Type deriving (Show, Eq)
-- subVar (v :->: _) = v
-- subVal (_ :->: v) = v

-- findSub :: Type -> [Sub] -> Maybe Sub 
-- {-# INLINE findSub #-}
-- findSub x@(TVar i) subs = find ((==x) . subVar) subs
-- findSub _ _ = Nothing

-- applySub :: Sub -> Type -> Type
-- applySub (var :->: val) t | var == t  = val
-- applySub s (Map t1 t2) = Map (applySub s t1) (applySub s t2)
-- applySub _ t = t

-- applySubs :: [Sub] -> Type -> Type
-- {-# INLINE applySubs #-}
-- applySubs ss t = if null toApply
--                    then t
--                    else applySubs ss (foldl (\t s -> applySub s t) t toApply)
--     where toApply = filter (inType t) ss
--           inType a@(TVar _) (b :->:  _) = a == b 
--           inType a@(Map c d) r@(b :->: _) = a == b || inType c r 
--                                            || inType d r
--           inType x _ = False

-- -- | Unify two types and return the subsitution that unifies them.
-- -- unify :: Type -> Type -> ThrowsError [Sub]
-- -- unify t1 t2 = unify_ t1 t2 []

-- -- unify_ :: Type -> Type -> [Sub] -> ThrowsError [Sub]
-- -- unify_ x y sub | x == y       = Right sub
-- unify_ x@(TVar i) y sub       = unify_var x y sub
-- unify_ x y@(TVar i) sub       = unify_var y x sub
-- unify_ (Map x1 x2) (Map y1 y2) sub = unify_ x1 y1 sub 
--                                      >>= \s -> unify_ (applySubs s x2) (applySubs s y2) s
                                               
-- unify_ x y _ = Left $ TypeError $ "Unification failed: cannot unify_ " ++ 
--               show x ++ " and " ++ show y

-- unify_var :: Type -> Type -> [Sub] -> ThrowsError [Sub]
-- unify_var var@(TVar i) x sub 
--     = case findSub var sub of
--         (Just (_ :->: val)) -> unify_ val x sub
--         Nothing -> case findSub x sub of
--                      (Just (_ :->: val)) -> unify_ var val sub
--                      Nothing -> if occur_check var x 
--                                 then Left $ TypeError $ "Unification failed: cannot unify_ " ++ 
--                                      show var ++ " and " ++ show x
--                                 else Right $ (var :->:  x):sub

-- occur_check x a@(TVar i) = x == a 
-- occur_check x (Map e1 e2) = occur_check x e1 || occur_check x e2
-- occur_check x _  = False

-- standardize_apart x y = y `addToTVars` (maxTVar x + 1)
--     where addToTVars !(TVar i) !j = (TVar (i + j))
--           addToTVars !(Map e1 e2) !j = Map (addToTVars e1 j)
--                                          (addToTVars e2 j)
--           addToTVars x _ = x

-- maxTVar (TVar i) = i
-- maxTVar (Map e1 e2) = max (maxTVar e1) (maxTVar e2)
-- maxTVar _ = 0

-- typeCheckTypeApp :: Type -> Type -> ThrowsError Type
-- typeCheckTypeApp t1 t2 = do
--   let beta = standardize_apart t2 (TVar 0)
--       t2'@(Map _ beta') = standardize_apart t1 (Map t2 beta)
--   subV <-   unify t1 t2'
--   return $ applySubs subV beta'
