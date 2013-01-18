-- DigitalArithmetic

module Experiments.DigitalArithmetic where

import Data.Maybe
import Debug.Trace

import Experiment
import Type

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

numto1dig n = n `mod` 2 
numto2dig n = let a = n `div` 1 `mod` 2
                  b = (n-a*1) `div` 2 `mod` 2
              in (b, a)
numto3dig n = let a = n `div` 1 `mod` 2
                  b = (n-a * 1) `div` 2 `mod` 2
                  c = (n-a * 1 - b*2) `div` 4 `mod` 2
              in (c, b, a)
numto4dig n = let a = n `div` 1 `mod` 2
                  b = (n-a * 1) `div` 2 `mod` 2
                  c = (n-a * 1 - b*2) `div` 4 `mod` 2
                  d = (n-a * 1 - b*2 - c*4) `div` 16 `mod` 2
              in (d, c, b, a)
numto5dig n = let a = n `div` 1 `mod` 2
                  b = (n-a * 1) `div` 2 `mod` 2
                  c = (n-a * 1 - b*2) `div` 4 `mod` 2
                  d = (n-a * 1 - b*2 - c*4) `div` 16 `mod` 2
                  e = (n-a * 1 - b*2 - c*4 - d*16) `div` 32 `mod` 2
              in (e, d, c, b, a)



