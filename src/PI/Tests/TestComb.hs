-- TestComb.hs

module TestComb where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad

import CL
import StdLib (stdlibTrie)
import qualified CombTrie as CT

instance Arbitrary Comb where
    arbitrary = sized combgen

combgen 0 = oneof $ map return (CT.keys stdlibTrie)
combgen n | n < 10 = liftM3 CApp subcomb subcomb arbitrary
          where subcomb = combgen (n `div` 2)
combgen n = combgen 5