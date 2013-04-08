{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad
import Test.Framework.TH
import Test.QuickCheck 
import Test.Framework.Providers.QuickCheck2

-- package modules
import Strappy.Type
import Strappy.CL
import Strappy.Expr
import qualified Strappy.CombMap as CM
import Strappy.StdLib

instance Arbitrary TyVar where
    arbitrary = liftM2 TyVar (liftM (enumId . abs) arbitrary) (return Star)


typegen 0 = oneof [ return tInt, 
                    liftM TVar arbitrary
                    ]
typegen n | n > 0 = oneof [ liftM2 (->-) subtype subtype]
          where subtype = typegen (n `div` 2)

instance Arbitrary Type where
    arbitrary = sized typegen

prop_mkTVar :: Int -> Bool
prop_mkTVar i = enumId i == k
    where (TVar (TyVar k _)) = mkTVar i

prop_applyNullSubst :: Type -> Bool
prop_applyNullSubst t = (apply nullSubst t) == t

prop_mergeSameSubst :: Subst -> Bool
prop_mergeSameSubst s = merge s s == Just (s ++ s)

prop_match t1 t2 = case match t1 t2 of
                     Just s -> apply s t1 == t2 
                     Nothing -> True

----------------------------------------------------------------------
-- Testing combinators (CL.hs)
----------------------------------------------------------------------
instance Arbitrary Comb where
    arbitrary = sized combgen

combgen 0 = oneof $ map return (CM.elems stdlib')
combgen n | n < 10 = liftM4 CApp subcomb subcomb (return tInt) (return 0)
          where subcomb = combgen (n `div` 2)
combgen n = combgen 5

prop_mkAppDepth c1 c2 = mkAppDepth c1 c2 == mkAppDepth c2 c1

prop_num2C i = let (N j) = reduceComb (num2C i)
               in i == j

prop_bool2C b = let (B c) = reduceComb (bool2C b)
                in c == b

prop_dOp2C i j = let op = dOp2C "+" (+) 
                     (N k) = reduceComb $ app' (app' op (num2C i)) (num2C j)
                 in k == i + j

main :: IO ()
main = $defaultMainGenerator