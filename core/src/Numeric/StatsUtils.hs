-- |
-- Module:      Numeric.StatsUtils
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- Numeric functions often needed for statistical applications. 

module Numeric.StatsUtils (
    -- * Sampling functions
    flipCoin,
    sampleMultinomial,
    sampleMultinomialNogen,
    -- * Log-Sum-Exp tricks
    logSumExp,
    logSumExpList,
    -- Working with distributions
    normalizeDist,
    entropyLogDist,
    -- * Misc
    logistic,
    log2
    ) where

import Control.Monad.Random

-- | Flip a coin with probability p in [0, 1]
flipCoin :: (Num a, Ord a, Random a, MonadRandom m) => a -> m Bool
flipCoin p = do r <- getRandomR (0, 1)
                return $ r < p

-- | Sample from a multinomial probability distribution.
-- The distribution is specified as a list of pairs (x, log p(x)) 
sampleMultinomial :: MonadRandom m => [(b, Double)] -> m b
sampleMultinomial dist = do r <- getRandomR (0, 1)
                            return $ sampleMultinomialNogen dist r

-- | As in sampleMultinomial, but takes in a random number in [0,1)
-- The distribution is specified as a list of pairs (x, log p(x))
sampleMultinomialNogen :: [(b, Double)] -> Double -> b
sampleMultinomialNogen dist rnd = sample rnd dist
    where sample r ((a, p):rest) = if r <= p then a 
                                      else sample (r - p) rest
          sample _ _ = error $ "Error when sampling from distribution: " ++ show (map snd dist)

-- | Return log(sum([exp(x1), ..., exp(xn)])), avoiding floating point underflow.
logSumExpList :: [Double] -> Double
logSumExpList [] = error "LogSumExp: invalid argument: []"
logSumExpList xs = foldl1 logSumExp xs

-- | Return log(exp(x) + exp(y)) without running into floating point
-- underflow issues.
logSumExp :: Double -> Double -> Double
logSumExp x y | isInvalidNum x = y
logSumExp x y | isInvalidNum y = x
logSumExp x y | x > y = x + log (1 + exp (y-x))
logSumExp x y = y + log (1 + exp (x-y))

isInvalidNum :: Double -> Bool
isInvalidNum x = isNaN x 


-- | Calculates the entropy of a discrete distribution, given log
-- probabilities
entropyLogDist :: [Double] -> Double
entropyLogDist ls =
  let logZ = logSumExpList ls
      ls' = [x - logZ | x <- ls]
      ls'' = filter (not . isInfinite) ls' 
  in negate $ sum $ [ exp logp * logp | logp <- ls'']

-- | Normalize a discrete probability distribution, representing the
-- probability in log units.
normalizeDist :: [(a, Double)] -> [(a, Double)]
normalizeDist dist
    = let logTotalMass = logSumExpList (map snd dist)
      in [(c, v - logTotalMass) | (c, v) <- dist]

-- | Evaluate a logistic function. 
logistic :: Double -- ^ x-shift
         -> Double -- ^ growth rate
         -> Double -- ^ x value
         -> Double -- ^ y value
logistic a b x = 1 / (1 + exp (-b * (x - a)))

-- | Natural logarithm of 2
log2 = log 2.0







