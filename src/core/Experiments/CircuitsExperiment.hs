-- BooleanCircuits.hs
{-# Language ParallelListComp #-} 

module Experiments.CircuitsExperiment where

import Control.Monad.State
import Data.Maybe
import qualified Data.HashMap as HMap
import Control.Monad.Random

import BooleanCircuits
import Type
import CL
import Expr
import Task
import StdLib
import Data
import qualified CombMap as CM
import CombMap (CombMap)
import Grammar
import Routers 
import PostProcess
import EnumBF
import ParseCL hiding (eval)
import Run
import DigArith
import Experiment

-- import Visualize



lib = (CM.fromList $ 
         [
          ("I", cI)
--         , ("True", cTrue)
--        , ("False", cFalse)
         ,("nand", cNand)
--         , ("&", cAnd)
--         , ("not", cNot)
--         , ("pair", cPair)
--         , ("fst", cFst)
--         , ("snd", cSnd)
--         , ("triple", cTriple)
--         , ("fst3", cFst3)
--         , ("snd3", cSnd3)
--         , ("thrd3", cThrd3)
           ]) 

lib1 = lib `CM.union` one_routers

jitteredGrammarFromLib :: MonadRandom m => HMap.Map k Comb -> m Grammar
jitteredGrammarFromLib lib = do 
  vs <- getRandomRs (-1.0,1.0)
  let lib' = CM.fromList [(c, 2 + k) | c <- CM.elems lib | k <- vs]
  return $ normalizeGrammar $ Grammar lib' 0

mkBoolCircuitsExperiment :: MonadRandom m => String 
                          -> HMap.Map String Comb -- ^ lib
                          -> Int -- ^ frontier size
                          -> [Task]
                          -> m Experiment
mkBoolCircuitsExperiment name lib frontierSize taskSet
    = do lib' <- jitteredGrammarFromLib lib
         return $ Experiment {
                      expName = name,
                      expTaskSet = taskSet,
                      expEps = 0,
                      expPrior = lib',
                      expInitLib = lib',
                      expDepthBound=3,
                      expNumBound = frontierSize,
                      expReps=15}


-- main :: IO ()
-- main = do
--   xs <- sampleCircuitTasks  dNumInputs dNumGates dGateType 200
--   let (circuits, tasks) = unzip xs
--   exp <- mkBoolCircuitsExperiment "200circuits_frontier1000" lib1 1000 tasks
--   runExp exp