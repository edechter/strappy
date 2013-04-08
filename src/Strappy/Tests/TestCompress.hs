-- TestCompress.hs

module TestCompress where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import TestUtils 

import Compress
import Grammar
import Type
import Task
import ParseCL (parseExprStd)
import qualified CombMap as CM

compressTestGroup = testGroup "Compress Testing Group" [
                    testCase "test compress" test_compress
                   ]


fromRight (Right x) = x

c1 = fromRight $ parseExprStd "1"
c2 = fromRight $ parseExprStd "+ 1 1"
c3 = fromRight $ parseExprStd "(+ 1) (+ 1 1)"
c4 = fromRight $ parseExprStd "((+ ((+ 1) 1)) ((+ 1) 1))"



test_compress = compress cs @?= CM.empty
    where cs = [c1, c2, c3, c4]

          
          





















