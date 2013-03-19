-- EX2.hs

module Experiments.EX2 where

import Run
import Experiments.CircuitsExperiment 
import BooleanCircuits


main :: IO ()
main = do 
  tasks <- readTasksFromCircuits "data/EX2/circuitTasks.txt"
  exp1000 <- mkBoolCircuitsExperiment "ex2_circuitlearning_1000"
               lib1
                1000
                tasks
  exp5000 <- mkBoolCircuitsExperiment "ex2_circuitlearning_5000"
               lib1
                5000
                tasks
  exp10000 <-mkBoolCircuitsExperiment "ex2_circuitlearning_10000"
               lib1
                10000
                tasks
  sequence $ map (\x -> runExp x "data/EX2") [exp1000, exp5000, exp10000]
  return ()