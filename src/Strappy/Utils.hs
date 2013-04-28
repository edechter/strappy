
module Strappy.Utils where

import Prelude hiding (flip)
import Control.Monad.Random 

flip :: (Num a, Ord a, Random a, MonadRandom m) => a -> m Bool
flip p = do r <- getRandomR (0, 1)
            return $ r < p

sampleMultinomial :: (Num a, Ord a, Random a, MonadRandom m) => [(b, a)] -> m b
sampleMultinomial dist = do r <- getRandomR (0, 1)
                            return $ sample r dist
    where sample r ((a, p):rest) = if r <= p then a else 
                                       sample (r - p) rest
          


logsumexp xs = a + (log . sum . map (exp . (\x -> x-a)) $ xs)
    where a = maximum xs

normalizeDist dist
    = let logTotalMass = logsumexp (map snd dist)
      in [(c, v - logTotalMass) | (c, v) <- dist]



