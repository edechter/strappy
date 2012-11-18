
{-# Language GeneralizedNewtypeDeriving #-}

module CL where 

import qualified Data.Set as Set
import Data.Map ((!), keys)
import Data.List (find, intersect)
import qualified Text.ParserCombinators.Parsec as P
import Text.Parsec.Token (parens)
import Text.ParserCombinators.Parsec ((<|>))
import Control.Applicative ((<$>), (<*>), (*>), (<*))


import Expr
                          
-- | define a type scheme
data Type = Map Type Type | Rtype | Btype | TVar Int deriving (Eq, Ord)

instance Show Type where
    show (Map t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show Rtype = "R"
    show Btype = "B" 
    show (TVar i) = "a" ++ show i

-- | define a combinator tree (Comb)
data Comb = CApp Comb Comb
          | CNode {cName :: String,
                   cExpr :: Expr,
                   cType :: Type} deriving (Eq)
instance Show Comb where
    show (CApp c1 c2) = "(" ++ show c1 ++ " " ++ show c2 ++ ")"
    show (CNode n e t) = n

-- | define common combinators
cI = CNode "I" (Func id) (Map (TVar 0) (TVar 0))
cS = CNode "S" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) (App g x))) typeS 
    where typeS = Map t1 (Map t2 t3)
          t1 = Map a (Map b c)
          t2 = Map a b
          t3 = Map b c
          a = TVar 2
          b = TVar 1
          c = TVar 0
cB = CNode "B" (Func $ \f -> Func $ \g -> Func $ \x -> (App f (App g x))) typeB 
    where typeB = Map t1 (Map t2 t3)
          t1 = Map b c
          t2 = Map a b
          t3 = Map b c
          a = TVar 2
          b = TVar 1
          c = TVar 0
cC = CNode "C" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) g )) typeC 
    where typeC = Map t1 (Map t2 t3)
          t1 = Map a (Map b c)
          t2 = b
          t3 = Map a c
          a = TVar 2
          b = TVar 1
          c = TVar 0

num2C i = CNode (show i) (R i) Rtype
dOp2C opString op = 
    CNode opString (Func $ \(R x) -> Func $ \(R y) -> R $ op x y) 
              (Map Rtype (Map Rtype Rtype))



-- | Unification algorithm for types. Taken verbatim from Russell &
-- Norvig Chapter 9 fig. 9.1.

data Sub = Sub {leftSub :: Type, rightSub :: Type} deriving (Show, Eq)

findSub x@(TVar i) sub = find ((==x) . leftSub) sub
findSub _ _ = Nothing

applySub :: Sub -> Type -> Type
applySub s@(Sub outT inT) t | outT == t = inT
applySub s@(Sub outT inT) (Map t1 t2) = Map (applySub s t1) (applySub s t2)
applySub _ t = t

applySubs :: [Sub] -> Type -> Type
applySubs ss t = if null toApply
                   then t
                   else applySubs ss (foldl (\t s -> applySub s t) t toApply)
    where toApply = filter (inType t) ss
          inType a@(TVar _) (Sub b _) = a == b 
          inType a@(Map c d) r@(Sub b _) = a == b || inType c r 
                                           || inType d r
          inType x _ = False



unify :: Type -> Type -> Either String [Sub]
unify t1 t2 = unify_ t1 t2 []
--    where t2' = standardize_apart t1 t2

unify_ :: Type -> Type -> [Sub] -> Either String [Sub]
unify_ x y sub | x == y       = Right sub
unify_ x@(TVar i) y sub       = unify_var x y sub
unify_ x y@(TVar i) sub       = unify_var y x sub
unify_ (Map x1 x2) (Map y1 y2) sub = unify_ x1 y1 sub >>= \s -> unify_ x2 y2 s
unify_ x y _ = Left $ "Unification failed: cannot unify_ " ++ 
              show x ++ " and " ++ show y

unify_var :: Type -> Type -> [Sub] -> Either String [Sub]
unify_var var@(TVar i) x sub 
    = case findSub var sub of
        (Just (Sub _ val)) -> unify_ val x sub
        Nothing -> case findSub x sub of
                     (Just (Sub _ val)) -> unify_ var val sub
                     Nothing -> if occur_check var x 
                                then Left $ "Unification failed: cannot unify_ " ++ 
                                     show var ++ " and " ++ show x
                                else Right $ (Sub var x):sub

occur_check x (Map a@(TVar i) b@(TVar j)) = x == a || x == b 
occur_check x (Map e1 e2) = occur_check x e1 || occur_check x e2
occur_check x _  = False

standardize_apart x y = y `addToTVars` (maxTVar x + 1)
    where addToTVars (TVar i) j = (TVar (i + j))
          addToTVars (Map e1 e2) j = Map (addToTVars e1 j)
                                         (addToTVars e2 j)
          addToTVars x _ = x

maxTVar (TVar i) = i
maxTVar (Map e1 e2) = max (maxTVar e1) (maxTVar e2)
maxTVar _ = 0
          
          
-- | Damas Milner unification algorithm                                          
typeCheck :: Comb -> Either String Type
typeCheck (CNode _ _ t) = Right t
typeCheck (CApp c1 c2) = do
  t1 <- typeCheck c1
  t2 <- typeCheck c2
  let beta = standardize_apart t2 (TVar 0)
      t2'@(Map _ beta') = standardize_apart t1 (Map t2 beta)
  subV <- unify t1 t2'
  return $ applySubs subV beta'

comb2Expr c@(CApp c1 c2) = do typeCheck c
                              e1 <- comb2Expr c1
                              e2 <- comb2Expr c2
                              return $ App e1 e2
comb2Expr c@(CNode _ e _) = return e
                               

      
--- TEST ---
sqr_c = (CApp (CApp cS (dOp2C "*" (*))) cI)
sqr_e = comb2Expr sqr_e


