module Strappy.Number where

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

-- | TASK SPECIFIC UTILITIES | --

xHack = 1
oHack = 2

logInt = log . fromIntegral

world2Expr :: [Int] -> Expr
world2Expr = intListToExpr

makeBag :: Int -> Int -> [Int]
makeBag x o = (replicate x xHack) ++ (replicate o oHack)

checkTask :: String      -- ^ determiner string
          -> String      -- ^ noun string
          -> Int         -- ^ number of x's
          -> Int         -- ^ number of o's
          -> Maybe [Int] -- ^ output of the task
          -> Bool        -- ^ whether task output is correct
checkTask _ _ _ _ Nothing = False
checkTask "all" noun x o (Just result) =
    case noun of
        "X"     -> result == replicate x xHack
        "O"     -> result == replicate o oHack
        "thing" -> (length $ filter (== xHack) result) == x &&
                   (length $ filter (== oHack) result) == o &&
                   (length result) == (x+o)
        _       -> False
checkTask "one" noun x o (Just result) =
    case noun of
        "X"     -> result == [xHack]
        "O"     -> result == [oHack]
        "thing" -> result == [xHack] || result == [oHack]
        _       -> False
checkTask "two" noun x o (Just result) =
    case noun of
        "X"     -> result == [xHack,xHack]
        "O"     -> result == [oHack,oHack]
        "thing" -> result == [xHack,xHack] || result == [oHack,oHack] ||
                   result == [xHack,oHack] || result == [oHack,xHack]
        _       -> False

checkSuperTask :: [(String, String, Int, Int, Maybe [Int])] -> Int
checkSuperTask result = sum $ map (\(det,noun,x,o,eval) -> bool2Binary $ checkTask det noun x o eval) result

-- | TASKS | --

makeTask :: String -> String -> Int -> Int -> EMTask
makeTask det noun x o =
    EMTask { etName = intercalate "_" ["task", det, noun, (show x), (show o)],
             etLogLikelihood = \e ->
                let results = [timeLimitedEval
                                  (e <> 
                                      (cTriple             <>
                                      (stringToExpr det)   <>
                                      (stringToExpr  noun) <>
                                      (world2Expr world))) |
                                      world <- permutations $ makeBag x o ] :: [Maybe [Int]]
                in (logInt $ sum (map (bool2Binary . checkTask det noun x o) results)) - (logInt $ length results),
             etType = tTriple (tList tChar) (tList tChar) (tList tInt) ->- (tList tInt) }

makeSuperTask :: EMTask
makeSuperTask =
    EMTask { etName = "super_task",
             etLogLikelihood = \e ->
                let results = [
                                [ (det,noun,x,o,timeLimitedEval
                                    (e <> 
                                        (cTriple             <>
                                        (stringToExpr det)   <>
                                        (stringToExpr  noun) <>
                                        (world2Expr world))))|
                                        world <- permutations $ makeBag x o] |
                                det <- ["one","two","all"],
                                noun <- ["X","O","thing"],
                                x    <- [1..3],
                                o    <- [1..3]] :: [[(String,String,Int,Int,Maybe [Int])]]
                in (logInt $ sum (map checkSuperTask results)) - (logInt . length $ concat results),
             etType = tTriple (tList tChar) (tList tChar) (tList tInt) ->- (tList tInt) }

seedGrammar :: Grammar
seedGrammar = Grammar { grApp = log 0.375,
                        grExprDistr = Map.fromList 
                        [ (annotateRequested e, 1.0) | e <- numberExprs ] }

-- | MAIN | --

main = do
    args@[rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
    putStrLn $ "Number (EC) run with: " ++ unwords args 
    setStdGen $ mkStdGen $ read rndSeed
    let seed = Grammar { grApp = log 0.375,
                         grExprDistr = Map.fromList 
                             [ (annotateRequested e, 1.0) | e <- numberExprs ] }
        tasks = [ makeTask det noun x o | det <- ["one","two","all"],
                                          noun <- ["X","O","thing"],
                                          x    <- [1..3],
                                          o    <- [1..3] ] ++
                [ makeSuperTask ]
    good <- loopM seed [0..19] $ \grammar step -> do
        putStrLn $ "EM Iteration: " ++ show step
        grammar' <- doEMIter (prefix++"/best_"++show step) (prefix++"/task_"++show step)
          "task_one_O_3_2" tasks (read lambda) (read pseudocounts) (read fSize) grammar  
        saveGrammar (prefix++"/grammar_" ++ show step) grammar'
        return grammar'
    putStrLn $ "Finished!"
