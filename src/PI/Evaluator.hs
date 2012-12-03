-- Evaluator.hs

module Evaluator where

import CL
import Expr
import Data

type Reward = Double
type Evaluator = (Datum -> Comb -> Reward)

-- | useful evaluators
mkEvalSingleEquality :: Int -> Evaluator
mkEvalSingleEquality rlimit d c 
    = abs $ a - b
      where Just (R a) = (reduceWithLimit rlimit $ comb2Expr' c)
            Just (R b) = reduceWithLimit rlimit $ comb2Expr' (head d)
      

