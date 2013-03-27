-- TestComb.hs

module TestComb where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad

import CL
import Type
import StdLib (stdlib', stdlib)
import qualified CombMap as CM

instance Arbitrary Comb where
    arbitrary = sized combgen

combgen 0 = oneof $ map return (CM.keys stdlib)
combgen n | n < 10 = liftM4 CApp subcomb subcomb (return tInt) (return 0)
          where subcomb = combgen (n `div` 2)
combgen n = combgen 5