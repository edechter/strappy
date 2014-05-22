module Main where

import Strappy.Expr
import Strappy.Type
import Strappy.Utils
import Strappy.Library
import Strappy.BeamSearch
import Strappy.Planner
import Strappy.EM
import Strappy.ProdFeat
import Strappy.Sample
import Strappy.EnumBits
import Strappy.Response

import qualified Data.Map as Map
import System.Environment
import System.Random
import Data.Maybe
import Control.Monad
import Debug.Trace
import Data.List
import Data.Array
import Data.Char

-- | TASK SPECIFIC UTILITIES | --

logInt = log . fromIntegral

checkTask2 :: (Char,Int,Int,Maybe Bool) -> Bool
checkTask2 (_,_,_,Nothing) = False
checkTask2 (t,x,y,(Just b))   =
    b == case t of
               's' -> (y == (x+1)) -- successor
               'p' -> (y == (x-1)) -- predecessor
               'e' -> (y ==  x   ) -- same as
               'l' -> (y <   x   ) -- less than
               'm' -> (y >   x   ) -- more than

checkTask1 :: (Char,Int,Maybe Int) -> Bool
checkTask1 (_,_,Nothing) = False
checkTask1 (t,x,(Just y)) = case t of
    's' -> y == (x+1) -- successor
    'p' -> y == (x-1) -- predecessor
    'e' -> y ==  x    -- same as
    'l' -> y <   x    -- less than
    'm' -> y >   x    -- more than
    _   -> False

makeNumberTask2 :: EMTask
makeNumberTask2 =
    EMTask { etName = "number_2",
             etLogLikelihood = \e ->
                let results = [(t,x,y, timeLimitedEval
                                   (e <> (cTriple <>
                                             (cChar2Expr t) <>
                                             (cInt2Expr  x) <>
                                             (cInt2Expr  y))))|
                                t <- "sp",
                                x <- [1..3],
                                y <- [1..3]] :: [(Char,Int,Int,Maybe Bool)]
                in (logInt $ sum [ bool2Binary $ checkTask2 r | r <- results ]) -
                   (logInt . length $ results),
             etType = tTriple tChar tInt tInt ->- tBool }

makeNumberTask1 :: EMTask
makeNumberTask1 =
    EMTask { etName = "number_1",
             etLogLikelihood = \e ->
                let results = [(t,x,timeLimitedEval (e <> (cPair <>
                                    (cChar2Expr t) <> (cInt2Expr  x)))) |
                                t <- "melps",
                                x <- [1..5]] :: [(Char,Int,Maybe Int)]
                in (logInt $ sum [ bool2Binary $ checkTask1 r | r <- results ]) -
                   (logInt . length $ results),
             etType = tPair tChar tInt ->- tInt }

main = do
    args@[rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
    putStrLn $ "Number (EC) run with: " ++ unwords args 
    setStdGen $ mkStdGen $ read rndSeed
    let seed = Grammar { grApp = log 0.375,
                         grExprDistr = Map.fromList 
                             [ (annotateRequested e, 1.0) | e <- numberExprs ] }
        tasks = [ makeNumberTask1 ]
    good <- loopM seed [0..19] $ \grammar step -> do
        putStrLn $ "EM Iteration: " ++ show step
        grammar' <- doEMIter (prefix++"/best_"++show step) tasks (read lambda) 
          (read pseudocounts) (read fSize) grammar  
        saveGrammar (prefix++"/grammar_" ++ show step) grammar'
        return grammar'
    putStrLn "Finished with EM Iterations"
