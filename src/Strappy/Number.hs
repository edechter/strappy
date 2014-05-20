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

mkBagExpr x = intListToExpr $ replicate x 1

binaryLog = log . fromIntegral . bool2Binary

checkTask :: [Char] -> [Char] -> Int -> Int -> Maybe Response -> Bool
checkTask _ _ _ _ Nothing = False
checkTask "i" t x y (Just (Nod b)) = 
    b == case t of
            "a" -> (y == (x+1)) -- successor
            "b" -> (y == (x-1)) -- predecessor
            "e" -> (y ==  x   ) -- same as
            "l" -> (y <   x   ) -- less than
            "m" -> (y >   x   ) -- more than
checkTask "i" t x y (Just (Give b)) = False
checkTask "i" t x y (Just (Speak s)) = False
-- checkTask "g" t x y _ = 
-- checkTask "w" t x y _ = 

-- | TASKS | -- 

makeNumberTask :: [Char] -> [Char] -> Int -> Int -> EMTask
makeNumberTask query task x y =
    EMTask { etName = intercalate "_" [query, task, show x, show y],
             etLogLikelihood = \e ->
                let queryExpr = charListToExpr query
                    taskExpr = charListToExpr task
                    result = timeLimitedEval (e <> queryExpr <> taskExpr <> (mkBagExpr x) <> (mkBagExpr y)) :: Maybe Response
                in binaryLog $ checkTask query task x y result,
             etType = tList tChar ->- tList tChar ->- tList tInt ->- tList tInt ->- tResponse }

main = do 
    args@[rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
    putStrLn $ "Number (EC) run with: " ++ unwords args 
    setStdGen $ mkStdGen $ read rndSeed
    let seed = Grammar { grApp = log 0.3625,
                         grExprDistr = Map.fromList 
                             [ (annotateRequested e, 1.0) | e <- numberExprs ] }
        tasks = [ makeNumberTask "i" t x y | t <- ["a", "b", "e", "l", "m"], 
                                             x <- [1..3],
                                             y <- [1..3] ]
    good <- loopM seed [0..19] $ \grammar step -> do
        putStrLn $ "EM Iteration: " ++ show step
        grammar' <- doEMIter (prefix++"/best_"++show step) tasks (read lambda) 
          (read pseudocounts) (read fSize) grammar  
        saveGrammar (prefix++"/grammar_" ++ show step) grammar'
        return grammar'
    putStrLn "Finished with EM Iterations"
