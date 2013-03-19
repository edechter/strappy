-- EX3.hs boolean circuits (snd try) difference from EX2: in EX2 the
-- circuit picking algorithm was biased towards very simple circuits.
-- This has been fixed. 

-- EX3.hs

module Experiments.EX3 where

import Run
import Experiments.CircuitsExperiment 
import BooleanCircuits


main :: IO ()
main = do 
  tasks <- readTasksFromCircuits "data/EX3/circuitTasks.txt"
  exp100 <- mkBoolCircuitsExperiment "ex3_circuitlearning_100"
               lib1
                100
                tasks
  exp500 <- mkBoolCircuitsExperiment "ex3_circuitlearning_500"
               lib1
                500
                tasks
  exp1000 <-mkBoolCircuitsExperiment "ex3_circuitlearning_1000"
               lib1
                1000
                tasks
  sequence $ map (\x -> runExp x "data/EX2") [exp100, exp500, exp1000]
  return ()