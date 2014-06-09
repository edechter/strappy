-- ApproxEq.hs

-- |
-- Module:      Numeric.ApproxEq
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- Tests for Numeric.StatsUtils

module Numeric.ApproxEq (
         (~=),
         approxEq,
         shouldBeApproxEq,
         approxEqDoubles,
         shouldBeApproxEqDoubles
       ) where

import Data.Foldable (Foldable)
import qualified Data.Foldable as Fold
import Test.HUnit.Lang

_TOLERANCE :: Double
_TOLERANCE = 1e-6

x ~= y = shouldBeApproxEq _TOLERANCE x y

approxEq :: (Show x, Num x, Ord x) => x -> x -> x -> Bool
approxEq tol x y = abs (x - y) < tol

shouldBeApproxEq :: (Show x, Num x, Ord x) => x -> x -> x -> IO ()
shouldBeApproxEq tol x y = if not $ approxEq tol x y
                              then assertFailure $ unlines [
                                    "Approximate equality failed:",
                                    "tol: " ++ show tol,
                                    "x: " ++ show x,
                                    "should be: " ++ show y]
                              else return ()
                                   
approxEqDoubles :: Double -> [Double] -> [Double] -> Double
approxEqDoubles tol xs ys = go 0 xs ys
  where go !acc [] [] = acc
        go !acc (x:xs) (y:ys) | x == infty && y == infty = go acc xs ys
        go !acc (x:xs) (y:ys) | x == negInfty && y == negInfty = go acc xs ys        
        go !acc (x:xs) (y:ys) = go (acc + abs (x - y)) xs ys
        go !acc _ _ = acc
        negInfty = read "-Infinity"
        infty = read "Infinity"

shouldBeApproxEqDoubles ::  Double -> [Double] -> [Double] -> IO ()
shouldBeApproxEqDoubles tol x y =
  if not (err < tol)
    then assertFailure $ unlines [
      "Approximate equality failed:",
      "tol: " ++ show tol,
      "x: " ++ show x,
      "should be: " ++ show y,
      "error: " ++ show err]
    else return ()
  where err = approxEqDoubles tol x y
