{-# Language GeneralizedNewtypeDeriving, BangPatterns #-}

module CL where 

-- | standard library imports
import qualified Data.Map as Map
import Data.Map ((!), keys)
import Control.Monad(foldM)
import Debug.Trace

-- | local imports
import Type
import CLError
import Expr
                          
-- | Define combinators -------
data Comb = CApp {rComb :: Comb, 
                  lComb ::  Comb,
                  cName :: String}
          | CNode {cName :: String,
                   cExpr :: Expr,
                   cType :: Type} 

instance Show Comb where
    show (CApp c1 c2 []) = "(" ++ show c1 ++ " " ++ show c2 ++ ")"
    show (CApp c1 c2 n) = n ++ ": " ++ "(" ++ show c1 ++ " " ++ show c2 ++ ")"
    show (CNode n _ _) = n

-- | An alternative to show: if combinator is named and evaluates to a
-- number or bool, show it an an evaluated expressions.
show' (CNode n _ _) = n
show' (CApp c1 c2 []) = "(" ++ show' c1 ++ " " ++ show' c2 ++ ")"
show' c@(CApp c1 c2 n) = case reduceComb c of
                           (R i) -> show i
                           (B b) -> show b
                           _     ->  "(" ++ show' c1 ++ " " ++ show' c2 ++ ")"

instance Eq Comb where
    (CApp c1 c2 _) == (CApp b1 b2 _) = (c1 == b1) && (c2 == b2)
    (CNode n _ _) == (CNode m _ _ ) = (n==m)
    a == b = False

-- | This equality operator ignores names of compound combinators.
eq' a@(CApp c1 c2 (n:ns)) b@(CApp b1 b2 (m:ms)) = eq' (CApp c1 c2 []) (CApp b1 b2 [])
eq' a b = a == b

-- -- | Extensional equality of two combinators. 
-- (<=>) :: Comb -> Comb -> Maybe Bool
-- c1 <=> c2 = case ( typeCheck' c1, typeCheck' c2) of
--               (Rtype, Rtype)
--                   -> Just $ reduceComb c1 == reduceComb c2
--               (Btype, Btype)
--                   -> Just $ reduceComb c1 == reduceComb c2
--               (Map Btype _, Map Btype _)
--                   -> let p1 = (CApp c1 (CNode "True" (B True) Btype) "", 
--                                CApp c1 (CNode "False" (B False) Btype) "") 
--                          p2 = (CApp c2 (CNode "True" (B True) Btype) "", 
--                                CApp c2 (CNode "False" (B False) Btype) "") 
--                      in do a <- fst p1 <=> fst p2
--                            b <- snd p1 <=> snd p2
--                            return $ a && b
--               _ -> Nothing

reduceComb :: Comb -> Expr
reduceComb = reduce . comb2Expr'

instance Ord Comb where 
    compare c1 c2 = compare (show c1) (show c2)

combDepth :: Comb -> Int
combDepth (CNode _ _ _) = 0
combDepth (CApp c1 c2 []) = 1 + max (combDepth c1) (combDepth c2)
combDepth (CApp c1 c2 _) = 0

num2C :: Double -> Comb 
num2C i = CNode (show i) (R i) Rtype

dOp2C :: String -> (Double -> Double -> Double) -> Comb
dOp2C opString op = CNode opString func (Map Rtype (Map Rtype Rtype))
    where func = Func $ \(R !x) -> Func $ \(R !y) -> R $ op x y

-- | Type checking combinators. 
typeCheck :: Comb -> TI Type
typeCheck (CNode _ _ t) = do s <- getSubst
                             return $ apply s t
typeCheck c@(CApp c1 c2 _ ) = do
  t1 <- typeCheck c1
  t2 <- typeCheck c2
  typeCheckApp t1 t2

-- ^ Create an instantiation of the combinator with a fresh type.
freshInstComb :: Comb -> TI Comb
freshInstComb c@(CNode _ _ _) = do t' <- freshInst $ cType c
                                   return $ c{cType=t'}
freshInstComb c@(CApp c1 c2 _ ) = foldM f c ts
    where ts = getTvs c
          f c s = do (TVar u) <- newTVar
                     let c' = sub s (TVar u) c 
                     return c'
          sub u t c@(CNode _ _ _) = if cType c == (TVar u) then c{cType=t} else c
          sub u t c@(CApp cl cr n) = CApp (sub u t cl) (sub u t cr) n
          getTvs (CNode _ _ t) = tv t
          getTvs (CApp cl cr _) = getTvs cl ++ getTvs cr
                                     

-- | Convert combinator to lambda expressions.
comb2Expr c@(CApp c1 c2 _ ) = do typeCheck c
                                 e1 <- comb2Expr c1
                                 e2 <- comb2Expr c2
                                 return $ App e1 e2
comb2Expr c@(CNode _ e _) = return e

traceIf c@(CApp c1@(CApp _ _ _)  c2 _ ) = if (=="PrimRec") .cName . rComb  $ c1  
            then (trace $ "c: " ++ show c) $ True else True
traceIf _ =  False

comb2Expr' c@(CApp c1 c2 _) = App (comb2Expr' c1) (comb2Expr' c2)
                           
comb2Expr' c@(CNode _ e _) = e

-- | Add library functions into TI monad
addLibToTI :: [Comb] -> TI [Comb]
addLibToTI (c:cc) = do c' <- freshInstComb c 
                       rest <- addLibToTI cc
                       return (c':rest)
addLibToTI [] = return []

isCombOfType :: Comb -> Type -> TI Bool
isCombOfType c t = do c' <- freshInstComb c
                      t' <- freshInst t
                      tc <- typeCheck c
                      unify' tc t'
                               
-- | Filter combinators by their types, using unification
filterCombinatorsByType :: [Comb] -> Type -> [TI Comb]
filterCombinatorsByType (c:cc) t 
    = let rest = filterCombinatorsByType cc t
          a = do succ <- isCombOfType c t
                 case succ of 
                   True-> return $ Just c
                   False -> return $ Nothing
      in case runTI a of 
           (Just v) -> (a >> return v) : rest
           otherwise -> rest
filterCombinatorsByType [] tp = []

      



      

