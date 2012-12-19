{-# Language GeneralizedNewtypeDeriving, BangPatterns #-}

module CL where 

-- | standard library imports
import qualified Data.Map as Map
import Data.Map ((!), keys)
import Data.List (union)
import Control.Monad 
import Control.Monad.Trans.Class
import Debug.Trace

-- | local imports
import Type
import CLError
import Expr
                          
-- | Define combinators -------
data Comb = CApp {rComb :: Comb, 
                  lComb ::  Comb,
                  cName :: String, 
                  cAppDepth :: Int }
          | CNode {cName :: String,
                   cExpr :: Expr,
                   cType :: Type
                   }
cDepth :: Comb -> Int
cDepth (CNode _ _ _) = 0
cDepth (CApp _ _ _ d) = d

mkAppDepth :: Comb -> Comb -> Int
mkAppDepth c1 c2 = 1 + (max (cDepth c1) (cDepth c2))

instance Show Comb where
    show (CApp c1 c2 [] _) = "(" ++ show c1 ++ " " ++ show c2 ++ ")"
    show (CApp c1 c2 n _) = n ++ ": " ++ "(" ++ show c1 ++ " " ++ show c2 ++ ")"
    show (CNode n _ _) = n

-- | An alternative to show: if combinator is named and evaluates to a
-- number or bool, show it an an evaluated expressions.
show' (CNode n _ _ ) = n
show' (CApp c1 c2 [] _) = "(" ++ show' c1 ++ " " ++ show' c2 ++ ")"
show' c@(CApp c1 c2 n _) = case reduceComb c of
                             (N i) -> show i
                             (C c) -> show c
                             _     ->  "(" ++ show' c1 ++ " " ++ show' c2 ++ ")"

instance Eq Comb where
    (CApp c1 c2 _ dl) == (CApp b1 b2 _ dr) = (dl == dr) && (c1 == b1) && (c2 == b2)
    (CNode n _ _) == (CNode m _ _ ) = (n==m)
    a == b = False

-- | This equality operator ignores names of compound combinators.
eq' a@(CApp c1 c2 (n:ns) dl) b@(CApp b1 b2 (m:ms) dr ) = eq' (CApp c1 c2 [] dl) (CApp b1 b2 [] dr)
eq' a b = a == b

reduceComb :: Comb -> Expr
reduceComb = reduce . comb2Expr'

instance Ord Comb where 
    compare c1 c2 = compare (show c1) (show c2)

num2C :: Int -> Comb 
num2C i = CNode (show i) (N i) tInt

dOp2C :: String -> (Int -> Int -> Int) -> Comb
dOp2C opString op = CNode opString func (tInt ->-  tInt ->- tInt)
    where func = Func $ \(N !x) -> Func $ \(N !y) -> N $ op x y

-- | Type checking combinators. 
typeCheck :: Monad m => Comb -> TypeInfT m Type
typeCheck (CNode _ _ t) = do s <- getSubst
                             return $ apply s t
typeCheck c@(CApp c1 c2 _ _) = do
  t1 <- typeCheck c1
  t2 <- typeCheck c2
  typeCheckApp t1 t2

-- | get type outside type monad
getType :: Comb -> Type
getType (CNode _ _ t) = t
getType c@(CApp c1 c2 _ _) = toType (getType c1)

-- ^ Create an instantiation of the combinator with a fresh type.
freshInstComb :: Monad m => Comb -> TypeInfT m Comb
freshInstComb c@(CNode _ _ _) = do t' <- freshInst $ cType c
                                   v <- getVarInt
                                   return $ c{cType=t'}
freshInstComb c@(CApp _ _ _ _) 
    = do n <- getVarInt
         putVarInt $ max (ts_maxInt + 1) n
         foldM f c ts
    where ts = getTvs c
          ts_maxInt = case map (\(TyVar s k) -> readId s) ts of
                        [] -> 0
                        xs -> maximum xs
          f c s = do ctp <- typeCheck c
                     tv <- newTVar (kind ctp)
                     let c' = sub s tv c 
                     return c'

sub s (TVar u) c@(CNode _ _ t) = c{cType = apply [(s, TVar u)] t}
sub s t@(TVar u) c@(CApp cl cr n d) = CApp (sub s t cl) (sub s t cr) n d
                                      
getTvs (CNode _ _ t) = tv t
getTvs (CApp cl cr _ _ ) = getTvs cl `union` getTvs cr
                                     

-- | Convert combinator to lambda expressions.
comb2Expr c@(CApp c1 c2 _ _ ) = do typeCheck c
                                   e1 <- comb2Expr c1
                                   e2 <- comb2Expr c2
                                   return $ App e1 e2
comb2Expr c@(CNode _ e _) = return e

traceIf c@(CApp c1@(CApp _ _ _ _)  c2 _ _) = if (=="PrimRec") .cName . rComb  $ c1  
            then (trace $ "c: " ++ show c) $ True else True
traceIf _ =  False

comb2Expr' c@(CApp c1 c2 _ _ ) = App (comb2Expr' c1) (comb2Expr' c2)
                           
comb2Expr' c@(CNode _ e _) = e

-- | Filter combinators by their types, using unification
filterCombinatorsByType :: [Comb] -> Type -> TypeInfT [] Comb
filterCombinatorsByType cs t = do c <- lift cs
                                  c' <- freshInstComb c
                                  tc <- typeCheck c'
                                  unify tc t
                                  return c'
                                  


      



      

