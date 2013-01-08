-- TestCompress.hs

module TestCompress where

import Test.QuickCheck
import Test.HUnit
import Data.List 
import Control.Monad

import ParseCL
import StdLib (stdlib, stdlib')
import CL
import Type
import qualified CombMap as CM
import Compress
import Search
import Task

import TestComb

fromRight (Right x) = x

-- combinators
cI = fromRight $ parseExpr stdlib' "I"
cII = fromRight $ parseExpr stdlib' "I I"
cIII = fromRight $ parseExpr stdlib' "I (I I)"
c4 = fromRight $ parseExpr stdlib' "(I I) (I I)"

test1 = TestCase $ assertEqual "test 1" a b
    where a = CM.fromList [(cI, 2), (cII, 2), (cIII, 1)]
          b = compress [cI, cII, cIII]

prop_orderInvariant :: [Comb] -> Bool
prop_orderInvariant cs@(c:(c':xs)) = (compress cs)  == compress (reverse cs)
prop_orderInvariant _ = True

-- test greedy search
g = map (fromRight . parseExpr stdlib') 
t1 = mkSingleEqualityTask 100 1
t2 = mkSingleEqualityTask 100 4
t3 = mkSingleEqualityTask 100 9
t4 = mkSingleEqualityTask 100 16
t5 = mkSingleEqualityTask 100 25
y1 = g ["(* 1 1)", "S * I 1"]
y2 = g ["(* 2 2)", "S * I 2"]
y3 = g ["(* 3 3)", "S * I 3"]
y4 = g ["(* 4 4)", "S * I 4"]
y5 = g ["(* 5 5)", "S * I 5"]

xs = [(t1, y1), (t2, y2), (t3, y3), (t4, y4), (t5, y5)]

ys = gen xs (CM.empty, [])

summary = map (\(i, y) -> (length $ CM.keys i, y)) ys

best = greedy stdlib xs









