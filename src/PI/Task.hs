-- Task.hs

-- | A task is a function from a combinator to a reward. 
module Task where

import Expr
import CL
import Type

type Reward = Double
data Task =  Task {taskName :: String, 
                   task :: (Comb -> Reward),
                  taskType :: Type} 
type TaskSet = [Task]

instance Show Task where
    show (Task n _ _ ) = n


-- | task constructors

mkSingleEqualityTask:: Int -> Int -> Task
mkSingleEqualityTask rlimit i 
    = let f c = fromIntegral $ (abs $ a - i)
              where a = case reduceWithLimit rlimit $ comb2Expr c of
                          (Just (N x)) -> x
                          Nothing -> 2147483647                    
      in Task (show i) f tInt

showTaskCombAssignments :: [(Task, Comb)] -> String
showTaskCombAssignments = unlines . map f
    where f (t, c) = show t ++ ": " ++ show c

    

