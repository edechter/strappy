-- TestCompress.hs

module TestCompress where

import Test.QuickCheck
import Test.HUnit
import Data.List 
import Control.Monad

import ParseCL
import StdLib (stdlib, stdlibTrie)
import CL
import Type
import qualified CombMap as CM
import Compress




-- combinators
cI = runTI $ parseExpr stdlib "I"
cII = runTI $ parseExpr stdlib "I I"
cIII = runTI $ parseExpr stdlib "I (I I)"
c4 = runTI $ parseExpr stdlib "(I I) (I I)"

test1 = TestCase $ assertEqual "test 1" a b
    where a = CT.fromList [(cI, 2), (cII, 2), (cIII, 1)]
          b = compress [cI, cII, cIII]

prop_orderInvariant :: [Comb] -> Bool
prop_orderInvariant cs@(c:(c':xs)) = (compress cs)  == compress (reverse cs)
prop_orderInvariant _ = True




