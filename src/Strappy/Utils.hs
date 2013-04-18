
module Strappy.Utils where

import Prelude hiding (flip)
import Control.Monad.Random 

flip :: (Num a, Ord a, Random a, MonadRandom m) => a -> m Bool
flip p = do r <- getRandomR (0, 1)
            return $ r < p

sampleMultinomial :: (Num a, Ord a, Random a, MonadRandom m) => [(a, b)] -> m b
sampleMultinomial dist = do r <- getRandomR (0, 1)
                            return $ sample r dist
    where sample r ((p, a):rest) = if r <= p then a else 
                                       sample (r - p) rest
