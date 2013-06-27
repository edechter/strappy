-- | This file defines global configuration constants

module Strappy.Config where

-- | Should we sample from P(expr | typed) or should we use sampling as described in the IJCAI paper?
-- True = sample from P(expr | typed)
-- False = ijcai-style sampling
usePCFGWeighting :: Bool
usePCFGWeighting = False

-- | Include debugging output? (grammars, etc)
verbose :: Bool
verbose = False

-- | Prune the grammar?
-- Slightly degrades accuracy, but allows us to explore a slightly larger frontier
pruneGrammar :: Bool
pruneGrammar = False

-- | Sample by enumeration?
sampleByEnumeration :: Bool
sampleByEnumeration = False

-- | Size of the frontier enumerated, or sampled, during the "E" step
frontierSize :: Int
frontierSize = 10000