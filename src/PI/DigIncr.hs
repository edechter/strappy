-- DigIncr.hs

{-# Language ParallelListComp #-} 

module DigIncr where

import Control.Monad.State
import Data.Maybe
import qualified Data.HashMap as HMap
import Control.Monad.Random
import Debug.Trace
import Data.GraphViz

import Experiment
import Type

import Type
import CL
-- import Search
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
import Experiments.DigitalArithmetic hiding (main)
import Visualize


filename = "data/digarith_ec2_allbool2and3_overnight_wjittermany_2013-01-22_07-09-56.306334_UTC/grammars.csv"
frontierSize = 1000

augmentGrammar (Grammar lib _) (Grammar lib' _ ) = normalizeGrammar $ Grammar (lib `CM.union` lib') 0


pairLib = CM.fromList $ [
          ("pair", cPair)
         , ("fst", cFst)
         , ("snd", cSnd)]

pairGr = grammarFromLib pairLib

main = do
  grFromFile <- getGrammarFromFile lib1 filename
  let lib' = augmentGrammar grFromFile pairGr
      lib'' = truncateGrammar lib' 10
  putStr $ show lib''
  let expDigIncr = Experiment { expName = "dig arith",
                                expTaskSet = [taskIncr1],
                                expEps = 0,
                                expPrior = lib'',
                                expInitLib = lib'',
                                expDepthBound = 3,
                                expNumBound = frontierSize,
                                expReps = 1}
  runExp expDigIncr

