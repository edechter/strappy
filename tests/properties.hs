{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad
import Test.Framework.TH
import Test.QuickCheck 
import Test.Framework.Providers.QuickCheck2

-- package modules
import Strappy.Type

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

main :: IO ()
main = $defaultMainGenerator