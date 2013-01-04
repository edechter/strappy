-- TestGrammar.hs

module TestGrammar where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Grammar
import ParseCL (parseExprStd)
import qualified CombMap as CM

grammarTestGroup = testGroup "Grammar Testing Group" [
                    testCase "estimate grammar 1" test_estimateGrammar1,
                    testCase "estimate grammar 2" test_estimateGrammar2,
                    testCase "estimate grammar 3" test_estimateGrammar3
                   ]

fromRight (Right x) = x
c1 = fromRight $ parseExprStd "+ 1 1"
c2 = fromRight $ parseExprStd "+"
c3 = fromRight $ parseExprStd "+ 1 1"
c4 = fromRight $ parseExprStd "1"
c5 = fromRight $ parseExprStd "+ 2"
c6 = fromRight $ parseExprStd "I (+ 1 1)"
c7 = fromRight $ parseExprStd "2"
c8 = fromRight $ parseExprStd "I"

test_estimateGrammar1 = estimateGrammar [c1] @?= Grammar lib c
    where lib = CM.fromList $ [(c4, 2)]
          c = 2

test_estimateGrammar2 = estimateGrammar [c1, c1] @?= Grammar lib c
    where lib = CM.fromList $ [(c1, 2)]
          c = 0

test_estimateGrammar3 = estimateGrammar [c1, c1, c6] @?= Grammar lib c
    where lib = CM.fromList $ [(c1, 3)]
          c = 1





