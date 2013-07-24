-- Task.hs

-- | A task is a function from a combinator to a reward. 
module Strappy.Task where

import Strappy.Expr
import Strappy.Type
import Strappy.Utils

data Task =  Task {taskName :: String, 
                   task :: Expr -> Double,
                   taskType :: Type} 

type TaskSet = [Task]

instance Show Task where
    show (Task n _ _ ) = n

	
mkIntTask :: Int -> Task
mkIntTask i = Task (show i) tsk tInt
        where tsk expr = case timeLimitedEval expr of
                                Nothing -> 0
                                Just r -> exp $ negate $ fromIntegral $ abs $ r - i

