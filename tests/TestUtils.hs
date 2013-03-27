-- TestUtils.hs

module TestUtils where

import Control.Monad (unless)
import Test.HUnit.Base 

assertEqualDouble :: String -- ^ The message prefix 
                  -> Double      -- ^ delta           
                  -> Double      -- ^ The expected value 
                  -> Double      -- ^ The actual value
                  -> Assertion
assertEqualDouble preface delta expected actual =
  unless (abs (expected - actual) < delta) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual

a @?=~ b = assertEqualDouble "" (1e-6) b a