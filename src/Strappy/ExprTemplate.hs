
module Strappy.ExprTemplate where

-- | Expression Templates are expressions with unbound bits in them
-- For example, these are used in rewrite rule:
-- S ?1 ?2 ?3 --> (?1 ?3) (?2 ?3)
-- Convention: Negative variables, like ?-2, correspond to universally quanitified variables,
-- while positive variables, like ?2, correspond to existentially quantified variables.
-- So, as an example:
-- E = 'a'
-- K ?-1 ?-2 --> ?-1
-- => E = K 'a' ?-2 which is converted to K 'a' ?1

-- | TODO: Combine this with the type inference code,
-- making a new module that abstracts out the common code.

import Strappy.Expr

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.Map as M
import Data.List

-- | type inference monad
type ExprTemp = StateT (Int, -- ^ next expr var
                        M.Map Int Expr) -- ^ Union-Find substitution
runET :: Monad m => ExprTemp m a -> m a
runET = runETVar 0

runETVar :: Monad m => Int -> ExprTemp m a -> m a
runETVar nextTVar m =
  evalStateT m (nextTVar, M.empty)

eVar :: Int -> Expr
eVar i = mkTerm ("?" ++ show i) undefined $ error "Attempt to eval eVar"

-- Create an unbound type variable
mkEVar :: Monad m => ExprTemp m Expr
mkEVar = do
  (n, s) <- get
  put (n+1, s)
  return $ eVar n
  
-- Does not check to see if the variable is not already bound
bindEVar :: Monad m => Int -> Expr -> ExprTemp m ()
bindEVar var e = do
  (n, s) <- get
  put (n, M.insert var e s)

-- Applies the current substitution to the expr,
-- "chasing" bound expr variables.
-- Performs path compression optimization.
-- The smaller-into-larger optimization does not apply.
applyESub :: Monad m => Expr -> ExprTemp m Expr
applyESub e@(Term { eName = '?':v}) = do
  (_, s) <- get
  case M.lookup (read v) s of
    Just e' -> do e'' <- applyESub e'
                  (n', s') <- get
                  put (n', M.insert (read v) e'' s')
                  return e''
    Nothing -> return e
applyESub e@(App { eLeft = l, eRight = r }) = do
  l' <- applyESub l
  r' <- applyESub r
  return $ e { eLeft = l', eRight = r' }

-- Unification
-- Primed unifyE is for exprs that have already had the substitution applied
unifyE t1 t2 = do
  t1' <- applyESub t1
  t2' <- applyESub t2
  unifyE' t1' t2'
unifyE' (Term { eName = v}) (Term { eName = v'}) | v == v' = return ()
unifyE' (Term { eName = '?':v}) t | occursE (read v) t = lift $ fail "Occurs E check"
unifyE' (Term { eName = '?':v}) t = bindEVar (read v) t
unifyE' t (Term { eName = '?':v}) | occursE (read v) t = lift $ fail "Occurs E check"
unifyE' t (Term { eName = '?':v}) = bindEVar (read v) t
unifyE' (App {eLeft = l, eRight = r}) (App { eLeft = l', eRight = r'}) = do
  unifyE l l'
  unifyE r r'
unifyE' _ _ = lift $ fail "Could not unifyE"

-- Occurs check: does the variable occur in the expr?
occursE :: Int -> Expr -> Bool
occursE v (Term { eName = '?':v'}) = v == read v'
occursE v (App { eLeft = l, eRight = r}) = occursE v l || occursE v r

-- Ground a universally quantified expr by instantiating new expr vars
instantiateExpr :: Monad m => 
                   Expr -> ExprTemp m Expr
instantiateExpr e = do
  let evars = filter (<0) $ nub $ getEVars e -- Only take universally quantified variables
  newEVars <- mapM (const mkEVar) evars
  return $ applyEVarSub (zip evars newEVars) e
  
getEVars :: Expr -> [Int]
getEVars (Term { eName = '?':v}) = [read v]
getEVars (App { eLeft = l, eRight = r}) = getEVars l ++ getEVars r
getEVars _ = []

applyEVarSub :: [(Int,Expr)] -> Expr -> Expr
applyEVarSub sub e@(Term { eName = '?':v}) =
  case lookup (read v) sub of
    Nothing -> e
    Just e' -> e'
applyEVarSub sub e@(App { eLeft = l, eRight = r }) =
  e { eLeft = applyEVarSub sub l,
	  eRight = applyEVarSub sub r }
applyEVarSub _ e = e