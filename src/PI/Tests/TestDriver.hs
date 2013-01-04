-- TestDriver.hs

module TestDriver where

import TestType

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List


main = defaultMain tests

tests = [
        testGroup "Type Testing Group" [
                testProperty "mkTVar preserves assigned integer " 
                             prop_mkTVar,
                testProperty "applying null substitution doesn't do anything" 
                             prop_applyNullSubst
            ]
    ]
