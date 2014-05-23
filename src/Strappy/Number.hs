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

mixedBag :: Int -> Int -> Expr
mixedBag x o = stringToExpr $ (replicate x 'x') ++ (replicate o 'o')

checkTask :: (String,String,Int,Int,Maybe String) -> Bool
checkTask (_,_,_,_,Nothing) = False
checkTask ("all",noun,x,o,(Just result))   =
    case noun of
        "X"     -> result == replicate x 'x'
        "O"     -> result == replicate o 'o'
        "thing" -> (length $ filter (== 'x') result) == x &&
                   (length $ filter (== 'o') result) == o &&
                   (length result) == (x+o)
        _       -> False
checkTask ("one",noun,x,o,(Just result))   =
    case noun of
        "X"     -> result == "x"
        "O"     -> result == "o"
        "thing" -> result == "x" || result == "o"
        _       -> False

-- | TASK | --

makeTask :: EMTask
makeTask =
    EMTask { etName = "the_task",
             etLogLikelihood = \e ->
                let results = [(det,noun,x,o, timeLimitedEval
                                   (e <> (cTriple <>
                                             (stringToExpr det) <>
                                             (stringToExpr  noun) <>
                                             (mixedBag x o))))|
                                det  <- ["one"], -- ,"all"],
                                noun <- ["X","O","thing"],
                                x <- [1..3],
                                o <- [1..3]] :: [(String,String,Int,Int,(Maybe String))]
                in (logInt $ sum [ bool2Binary $ checkTask r | r <- results ]) -
                   (logInt . length $ results),
             etType = tTriple (tList tChar) (tList tChar) (tList tInt) ->- (tList tInt) }

-- | MAIN | --

main = do
    args@[rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
    putStrLn $ "Number (EC) run with: " ++ unwords args 
    setStdGen $ mkStdGen $ read rndSeed
    let seed = Grammar { grApp = log 0.375,
                         grExprDistr = Map.fromList 
                             [ (annotateRequested e, 1.0) | e <- numberExprs ] }
        tasks = [ makeTask ]
    good <- loopM seed [0..19] $ \grammar step -> do
        putStrLn $ "EM Iteration: " ++ show step
        grammar' <- doEMIter (prefix++"/best_"++show step) tasks (read lambda) 
          (read pseudocounts) (read fSize) grammar  
        saveGrammar (prefix++"/grammar_" ++ show step) grammar'
        return grammar'
    putStrLn "Finished with EM Iterations"
