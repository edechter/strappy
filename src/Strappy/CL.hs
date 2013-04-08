{-# Language GeneralizedNewtypeDeriving, BangPatterns #-}

module Strappy.CL where 

-- standard library imports
import qualified Data.Map as Map
import Data.Map ((!), keys)
import Data.List (union)
import Control.Monad 
import Control.Monad.Trans.Class
import Control.Monad.State

--  local imports
import Strappy.Type
import Strappy.CLError
import Strappy.Expr
                          
--  Define combinators -------
data Comb = CApp {lComb :: Comb, 
                  rComb ::  Comb,
                  cType :: Type,
                  cAppDepth :: Int,
                  cLabel :: Maybe String -- ^ the label of a
                                         -- combinator is a unique
                                         -- name for the subtree
                 }
          | CLeaf {cName :: String,
                   cExpr :: Expr,
                   cType :: Type}
          | CHole {cType :: Type} -- ^ a location in a tree 


cDepth :: Comb -> Int
cDepth CNode{} = 0
cDepth (CApp _ _ _ d) = d

mkAppDepth :: Comb -> Comb -> Int
mkAppDepth c1 c2 = 1 + max (cDepth c1) (cDepth c2)

isCNode (CNode{}) = True
isCNode _ = False

type SynthComb = Either String Comb
 
app :: SynthComb -> SynthComb -> SynthComb
app m1 m2 = do c1 <- m1
               c2 <- m2
               case getAppType c1 c2 of
                Left err -> Left err
                Right t -> let d = mkAppDepth c1 c2 
                           in Right $ CApp c1 c2 t d 

app' :: Comb -> Comb -> Comb
app' c1 c2 = case getAppType c1 c2 of
               Left err -> error $ "Error in app' in CL.hs: " ++ err
               Right t -> let d = mkAppDepth c1 c2
                              in CApp c1 c2 t d

infixl 4 <:>
(<:>) = app

infixl 4 <::>
(<::>) = \m1 c2 ->  m1 <:> (return c2)
                  

instance Show Comb where
    show (CApp c1 c2 _ _) = "(" ++ show c1 ++ " " ++ show c2 ++ ")"
    show (CNode n _ _) = n
    show (CTerminal t) =  "CTerm: " ++ show t

-- | An alternative to show: if combinator is named and evaluates to a
-- number or bool, show it an an evaluated expressions.
show' (CNode n _ _ ) = n
show' c@(CApp c1 c2 _ _) = case reduceComb c of
                             (N i) -> show i
                             (C c) -> show c
                             _     ->  "(" ++ show' c1 ++ " " ++ show' c2 ++ ")"

instance Eq Comb where
    (CApp c1 c2 _ dl) == (CApp b1 b2 _ dr) = (dl == dr) && (c1 == b1) && (c2 == b2)
    (CNode n _ _) == (CNode m _ _ ) = (n==m)
    a == b = False

reduceComb :: Comb -> Expr
reduceComb c =  reduce ( comb2Expr c)

instance Ord Comb where 
    compare c1 c2 = compare (show c1) (show c2)

num2C :: Int -> Comb 
num2C i = CNode (show i) (N i) tInt

dOp2C :: String -> (Int -> Int -> Int) -> Comb
dOp2C opString op = CNode opString func (tInt ->-  tInt ->- tInt)
    where func = Func $ \(N !x) -> Func $ \(N !y) -> N $ op x y

bool2C :: Bool -> Comb
bool2C c = CNode (show c) (B c) tBool

-- | get type outside type monad
getType :: Comb -> Either String Type
getType (CNode _ _ t) = Right t
getType c@(CApp c1 c2 _ _) = getAppType c1 c2
getType c@(CTerminal t) = Right t

getAppType :: Comb -> Comb -> Either String Type
getAppType c1 c2  
    = let st = do t1 <- lift (getType c1) >>= freshInst
                  t2 <- lift (getType c2) >>= freshInst
                  t <- newTVar Star
                  case mgu t1 (t2 ->- t) of 
                    Nothing -> lift $ Left $ "Type error: " ++ " unable to unify " 
                               ++ show t1 ++ " and " ++ show t2 
                               ++ " when attempting to apply " 
                              ++  show c1 ++ " to " ++ show c2
                    Just subst -> return $ toType (apply subst t1)
      in liftM fst $ runStateT st 0
                                         

sub s (TVar u) c@(CNode _ _ t) = c{cType = apply [(s, TVar u)] (cType c)}
                                      
getTvs (CNode _ _ t) = tv t
getTvs (CApp cl cr _ _ ) = getTvs cl `union` getTvs cr

comb2Expr c@(CApp c1 c2 _ _ ) = App (comb2Expr c1) (comb2Expr c2)
comb2Expr c@(CNode _ e _) = e

filterCombinatorsByType :: [Comb] -> Type -> StateT Int [] Comb
filterCombinatorsByType (c:cs) t  
    = do ct <- freshInst (cType c)
         case mgu ct t of
           Just sub -> (return  (c{cType = apply sub ct})) `mplus` rest
           Nothing -> rest
      where rest = filterCombinatorsByType cs t
filterCombinatorsByType [] t = lift []




                             

                                  


      



      

