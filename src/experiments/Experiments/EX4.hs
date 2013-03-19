-- EX4.hs


module Experiments.EX4 where

import Run
import Experiments.CircuitsExperiment 
import BooleanCircuits


main :: IO ()
main = do 
  tasks <- readTasksFromCircuits "data/EX4/circuitTasks.txt"
  exp100 <- mkBoolCircuitsExperiment "ex4_circuitlearning_100"
               lib1
                100
                tasks
  exp500 <- mkBoolCircuitsExperiment "ex4_circuitlearning_500"
               lib1
                500
                tasks
  exp1000 <-mkBoolCircuitsExperiment "ex4_circuitlearning_1000"
               lib1
                1000
                tasks
  sequence $ map (\x -> runExp x "data/EX4") [exp100, exp500, exp1000]
  return ()