-- TestGrammar.hs

module TestGrammar where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Grammar
import Type
import Task
import ParseCL (parseExprStd)
import qualified CombMap as CM

grammarTestGroup = testGroup "Grammar Testing Group" [
                    testCase "test counAlts 1" test_countAlts1,
                    testCase "test estimateGrammar 1" test_estimateGrammar
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


test_countAlts1 = countAlts cs (CM.empty) c tInt @?= 
                   (CM.fromList [(c3,1),(c8,3)], 0)
    where cs = [c3, c8]
          c = c6

test_estimateGrammar = estimateGrammar prior 5 ind [(t1, c)] 
                       @?= Grammar (CM.fromList [(c5, 0)]) 0
    where t1 = Task "" (\_ -> 0.0) (tInt ->- tInt)
          c = c5
          ind = CM.fromList [(c5, 1)]
          prior = Grammar (CM.fromList [(c8, 0)]) (- log 0.5)







