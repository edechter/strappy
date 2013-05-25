-- Task.hs

-- | A task is a function from a combinator to a reward. 
module Strappy.Task where

import Strappy.Expr
import Strappy.Type

data Task =  Task {taskName :: String, 
                   task :: UExpr -> Double,
                   taskType :: Type} 

type TaskSet = [Task]

instance Show Task where
    show (Task n _ _ ) = n

	
mkIntTask :: Int -> Task
mkIntTask i = Task (show i) tsk tInt
        where tsk uexpr = case safeEval (fromUExpr uexpr) of
                                Nothing -> negate (1.0/0.0)
                                Just r -> negate $ fromIntegral $ abs $ r - i

