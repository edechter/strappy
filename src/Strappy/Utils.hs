{-# LANGUAGE TupleSections  #-}
module Strappy.Utils where

import Prelude hiding (flip)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random
import Control.Monad.Maybe
import Debug.Trace
import Data.Array.IO 

flipCoin :: (Num a, Ord a, Random a, MonadRandom m) => a -> m Bool
flipCoin p = do r <- getRandomR (0, 1)
                return $ r < p

sampleMultinomial :: MonadRandom m => [(b, Double)] -> m b
sampleMultinomial dist = do r <- getRandomR (0, 1)
                            return $ sampleMultinomialNogen dist r

-- | As in sampleMultinomial, but takes in a random number in [0,1)
sampleMultinomialNogen :: [(b, Double)] -> Double -> b
sampleMultinomialNogen dist rnd = sample rnd dist
    where sample r ((a, p):rest) = if r <= p then a 
                                             else sample (r - p) rest
          sample _ _ = error $ "Error when sampling from distribution: " ++ show (map snd dist)

-- | As in sampleMultinomial, but takes in unnormalized log probabilities
sampleMultinomialLogProb :: MonadRandom m => [(b, Double)] -> m b
sampleMultinomialLogProb dist =
  -- Normalize
  let logZ = logSumExpList $ map snd dist
      dist' = map (\(thing, ll) -> (thing, exp (ll - logZ))) dist
  in sampleMultinomial dist'
  
-- | As in sampleMultinomialLogProb, but takes in a random number in [0,1)
sampleMultinomialLogProbNogen :: [(b, Double)] -> Double -> b
sampleMultinomialLogProbNogen dist rnd =
  -- Normalize
  let logZ = logSumExpList $ map snd dist
      dist' = map (\(thing, ll) -> (thing, exp (ll - logZ))) dist
  in sampleMultinomialNogen dist' rnd

-- | Is the number either NaN or infinite?
isInvalidNum :: Double -> Bool
isInvalidNum x = isNaN x || isInfinite x

logSumExpList :: [Double] -> Double
logSumExpList = foldl1 logSumExp

logSumExp :: Double -> Double -> Double
logSumExp x y | isInvalidNum x = y
logSumExp x y | isInvalidNum y = x
logSumExp x y | x > y = x + log (1 + exp (y-x))
logSumExp x y = y + log (1 + exp (x-y))

-- Calculates the entropy of a discrete distribution, given log probabilities
entropyLogDist :: [Double] -> Double
entropyLogDist ls =
  let ls' = map snd $ normalizeDist $ map (undefined,) ls
  in - (sum $ zipWith (*) ls' $ map exp ls')

-- | Just foldM with its arguments switched about
loopM :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
loopM start xs step = foldM step start xs

normalizeDist :: [(a, Double)] -> [(a, Double)]
normalizeDist dist
    = let logTotalMass = logSumExpList (map snd dist)
      in [(c, v - logTotalMass) | (c, v) <- dist]

logistic :: Double -- ^ x-shift
         -> Double -- ^ growth rate
         -> Double -- ^ x value
         -> Double -- ^ y value
logistic a b x = 1 / (1 + exp (-b * (x - a)))

flipOrdering :: Ordering -> Ordering
flipOrdering LT = GT
flipOrdering GT = LT
flipOrdering EQ = EQ


-- | Like fromJust, but with error reporting
safeFromJust :: String -> Maybe a -> a
safeFromJust str Nothing = error str
safeFromJust _ (Just x) = x

-- | Substitutes one value in a list for another
substituteInList :: Eq a => [a] -> a -> a -> [a]
substituteInList [] _ _ = []
substituteInList (x:xs) y z | x == y = z:xs
substituteInList (x:xs) y z = x : substituteInList xs y z


unwordsBy :: String -> [String] -> String
unwordsBy sep [x] = x
unwordsBy sep (x:xs) = x ++ sep ++ unwordsBy sep xs

forceShowHack :: Show a => a -> IO ()
forceShowHack x = do
  let strVal = show x
  a <- (newListArray (1::Int,length strVal) strVal) :: IO (IOUArray Int Char)
  return ()

fst3 (x, _, _) = x
snd3 (_, x, _) = x
thd3 (_, _, x) = x

instance (MonadRandom m) => MonadRandom (MaybeT m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs
