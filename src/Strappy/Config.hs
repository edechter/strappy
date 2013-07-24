-- | This file defines global configuration constants

module Strappy.Config where

import Data.Timeout

-- | Should we sample from P(expr | typed) or should we use sampling as described in the IJCAI paper?
-- True = sample from P(expr | typed)
-- False = ijcai-style sampling
usePCFGWeighting :: Bool
usePCFGWeighting = False

-- | Include debugging output? (grammars, etc)
verbose :: Bool
verbose = True

-- | Prune the grammar?
-- Slightly degrades accuracy, but allows us to explore a slightly larger frontier
pruneGrammar :: Bool
pruneGrammar = False

-- | Sample by enumeration?
sampleByEnumeration :: Bool
sampleByEnumeration = True

-- | Size of the frontier enumerated, or sampled, during the "E" step
frontierSize :: Int
frontierSize = 1500

-- | Number of plans sampled for each task
numberOfPlansPerTask :: Int
numberOfPlansPerTask = 50

-- | Length of each plan sampled
maximumPlanLength :: Int
maximumPlanLength = 3

-- | How much stochasticity should there be in the planning?
--   beta = 1/T
planningBeta :: Double
planningBeta = 1

-- | Timeout for evaluations
--   Measured in nanoseconds
maxEvalTime :: Timeout
maxEvalTime = fromIntegral 10000

-- | Truncated stick for variational inference
truncatedStickLen = 20