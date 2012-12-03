-- Task.hs

-- | A task is a function from a combinator to a reward. 
module Task where

import Expr
import CL
import Type

type Reward = Double
data Task =  Task {taskName :: String, 
                   task :: (Comb -> Reward)} 
type TaskSet = [Task]

instance Show Task where
    show (Task n _) = n


-- | task constructors

mkSingleEqualityTask:: Int -> Double -> Task
mkSingleEqualityTask rlimit i 
    = let f c = (-1) * (abs $ a - i)
              where Just (R a) = reduceWithLimit rlimit $ comb2Expr' c
      in Task (show i) f

    

