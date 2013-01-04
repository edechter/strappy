-- TestDriver.hs


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
                             prop_applyNullSubst,
                testProperty "merging 2 identical substitutions just concatenates them"
                             prop_mergeSameSubst,
                testCase "mgu of tInt w tInt is empty" test_mgu1,
                testCase "mgu of tInt w tBool is Nothing" test_mgu2,
                testCase "mgu 3" test_mgu3,
                testCase "mgu 4" test_mgu4,
                testCase "mgu 5" test_mgu5,
                testCase "mgu 6" test_mgu6
                
            ]
    ]
