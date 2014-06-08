
module Strappy.Experiments.Words.Likelihood where

import Text.EditDistance

levenshteinLogLikelihood :: Double -> String -> String -> Double
-- | Return a log likelihood score that is based on the negative edit 
-- | distance between the input strings. This value is then scaled by 
-- | the factor beta.
levenshteinLogLikelihood beta string1 string2 = 
	negate . (* beta) . fromIntegral $ levenshteinDistance defaultEditCosts string1 string2

