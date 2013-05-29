
module Strappy.Utils where

import Prelude hiding (flip)
import Control.Monad.Random 
import Data.List (null)

flip :: (Num a, Ord a, Random a, MonadRandom m) => a -> m Bool
flip p = do r <- getRandomR (0, 1)
            return $ r < p

sampleMultinomial :: (Num a, Ord a, Random a, MonadRandom m) => [(b, a)] -> m b
sampleMultinomial dist = do r <- getRandomR (0, 1)
                            return $ sample r dist
    where sample r ((a, p):rest) = if r <= p then a 
                                             else sample (r - p) rest
          
logsumexp xs = a + (log . sum . map (exp . (\x -> x - a)) $ xs)
    where a = if null xs then error "logsumexp: list argument must have length greater than zero, but got []." 
                         else maximum xs

normalizeDist dist
    = let logTotalMass = logsumexp (map snd dist)
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

