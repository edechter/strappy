-- | This file defines global configuration constants

module Strappy.Config where

-- | Should we sample from P(expr | typed) or should we use sampling as described in the IJCAI paper?
usePCFGWeighting :: Bool
usePCFGWeighting = True