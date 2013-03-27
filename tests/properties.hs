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

prop_mkTVar :: Int -> Bool
prop_mkTVar i = enumId i == k
    where (TVar (TyVar k _)) = mkTVar i

main :: IO ()
main = $defaultMainGenerator