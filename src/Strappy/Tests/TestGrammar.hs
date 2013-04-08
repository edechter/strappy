-- TestGrammar.hs

module TestGrammar where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import TestUtils 

import Grammar
import Type
import Task
import ParseCL (parseExprStd)
import qualified CombMap as CM

grammarTestGroup = testGroup "Grammar Testing Group" [
--                    testCase "test counAlts 1" test_countAlts1,
                    testCase "test estimateGrammar 1" test_estimateGrammar,
                    testCase "test bernLogProb 1" test_bernLogProb1,
                    testCase "test bernLogProb 2" test_bernLogProb2,
                    testCase "test calcLogProb 1" test_calcLogProb1
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
c9 = fromRight $ parseExprStd "+ 1"

-- test_countAlts1 = countAlts cs  c tInt @?= 
--                    CM.fromList []
--     where cs = [c3, c4]
--           c = c3

test_estimateGrammar = estimateGrammar prior 5 ind [(t1, c)] 
                       @?= Grammar (CM.fromList [(c5, 0), (c8, 0)]) 2
    where t1 = Task "" (\_ -> 0.0) (tInt ->- tInt)
          c = c5
          ind = CM.fromList [(c5, [tInt ->- tInt])]
          prior = Grammar (CM.fromList [(c8, 0)]) (- log 0.5)

test_bernLogProb1 = bernLogProb 5 10 @?=~ log (0.5)
test_bernLogProb2 = bernLogProb 5 5  @?=~ 0

test_calcLogProb1 = calcLogProb gr tp c @?=~ (-1.407606)
    where gr = Grammar lib ex
          lib = CM.fromList [(c8, (-2)), (c9, (-1))]
          ex = (-3)
          t0 = mkTVar 0
          tp = t0 ->- t0
          c = c8
          
          









