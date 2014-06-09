-- StatsUtils.hs

-- |
-- Module:      StatsUtilsSpec
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- Tests for Numeric.StatsUtils

module StrappyCoreTest.StatsUtilsSpec (spec)  where

import Control.Monad
import Control.Arrow
import Test.Hspec
import Test.HUnit.Lang

import Numeric.StatsUtils
import Numeric.ApproxEq

flipStats :: Double -> IO (Double, Double)
flipStats p = do xs <- replicateM 10000 (flipCoin p)
                 let heads = filter id xs
                     n = fromIntegral $ length xs
                     nHeads = fromIntegral $ length heads
                     nTails = n - nHeads
                     mean = nHeads / n
                     variance = nHeads * nTails / (n ** 2)
                 return (mean, variance)

spec :: Spec
spec = describe "Numeric.StatsUtils" $ do
  it "does not underflow" $ do
    logSumExpList (replicate 10 (log 0.1)) ~= log (1.0)
    logSumExpList (replicate 1000 (log 0.001)) ~= log (1.0)

  it "coin has correct mean and variance" $ do
    (mean, variance) <- flipStats 0.2
    shouldBeApproxEq 0.1 mean 0.2
    shouldBeApproxEq 0.1 variance (0.2 * 0.8)

  it "samples correctly from multinomial distribution" $ do
    xs <- replicateM 10000 $ sampleMultinomial $ map (second exp) $ normalizeDist $ map (second log) [("a", 0), ("b", 1), ("c", 2)]
    let nA = fromIntegral . length . filter (=="a") $ xs
        nB = fromIntegral . length . filter (=="b") $ xs
        nC = fromIntegral . length . filter (=="c") $ xs
        n  = fromIntegral . length $ xs
        pA = nA / n
        pB = nB / n
        pC = nC / n
    shouldBeApproxEq 1e-1 pA (0 :: Double)
    shouldBeApproxEq 1e-1 pB (0.333333 :: Double)
    shouldBeApproxEq 1e-1 pC (0.666666 :: Double)
    

  it "normalizes distributions" $ do
    let d = map (second log) [("a", 0), ("b", 1), ("c", 2)]
    let d' = map (second log) [("a", 0), ("b", 0.333), ("c", 0.666)]
    shouldBeApproxEqDoubles 0.1 (map snd $ normalizeDist d) (map snd d') 
    
  it "calculates entropy of discrete distributions" $ do
    let d = map log [0, 2, 0, 0]
        _H = 0.0
        d' = map log [10, 10, 10, 10]
        _H' = negate $ log (1/4)
    shouldBeApproxEq 0.0001 (entropyLogDist d) _H
    shouldBeApproxEq 0.0001 (entropyLogDist d') _H'
