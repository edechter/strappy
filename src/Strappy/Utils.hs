
module Strappy.Utils where

import Prelude hiding (flip)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random
import Control.Monad.Maybe
import Data.List (null)

flip :: (Num a, Ord a, Random a, MonadRandom m) => a -> m Bool
flip p = do r <- getRandomR (0, 1)
            return $ r < p

sampleMultinomial :: (Num a, Ord a, Random a, MonadRandom m) => [(b, a)] -> m b
sampleMultinomial dist = do r <- getRandomR (0, 1)
                            return $ sample r dist
    where sample r ((a, p):rest) = if r <= p then a 
                                             else sample (r - p) rest
          
logSumExpList :: [Double] -> Double
logSumExpList xs = a + (log . sum . map (exp . (\x -> x - a)) $ xs)
    where a = if null xs then error "logsumexp: list argument must have length greater than zero, but got []." 
                         else maximum xs

logSumExp :: Double -> Double -> Double
logSumExp x y | x > y = x + log (1 - exp (y-x))
logSumExp x y = y + log (1 - exp (x-y))

-- | Just foldM with its arguments switched about
loopM :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
loopM start xs step = foldM step start xs

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


instance (MonadRandom m) => MonadRandom (MaybeT m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs