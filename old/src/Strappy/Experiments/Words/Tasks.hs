{-# Language ScopedTypeVariables #-}

module Strappy.Experiments.Words.Tasks where

import Control.Exception
import Debug.Trace

import Strappy.Expr
import Strappy.Type
import Strappy.Planner
import Strappy.Library

import Strappy.Experiments.Words.Grammar
import Strappy.Experiments.Words.Likelihood

string2PlanTask :: Double    -- beta 
                -> String   
                -> PlanTask 
-- | Returns a PlanTask whose target string is the input string. *beta* scales
--the loglikelihood scoring function. Larger *beta* results in a steeper
--scoring function; lower *beta* results in a shallower one. 
string2PlanTask beta string = PlanTask string loglikelihood (tList tChar) cEmpty
    where loglikelihood' expr = levenshteinLogLikelihood beta string (eval expr :: String)
          loglikelihood expr = let x = loglikelihood' expr in 
          						catch (x `seq` return x) $ \(err :: SomeException) -> return (-10000) 
