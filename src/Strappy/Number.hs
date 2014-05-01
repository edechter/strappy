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

import qualified Data.Map as Map
import System.Environment
import System.Random
import Data.Maybe
import Control.Monad
import Debug.Trace
import Data.List
import Data.Array
import Data.Char
import Data.Set (Set())
import qualified Data.Set as Set

set2tSet :: Set Int -> Expr
set2tSet s = mkTerm (show s) (tSet tInt) s

bool2Binary :: Bool -> Int
bool2Binary x = if x then 1 else 0

makeNumberTask :: Int -> EMTask
makeNumberTask thisN = 
    EMTask { etName = "num_" ++ show thisN,
             etLogLikelihood = -- partial credit: LL is # correct/# total.
                 \e -> let result  = [ timeLimitedEval (e <> (set2tSet . makeIntSet) n) | n <- [1..10] ] :: [Maybe Bool]
                           correct = (map (\x -> Just (x == thisN)) [1..10]) :: [Maybe Bool]
                       in log (fromIntegral (sum $ zipWith (\x y -> bool2Binary $ x == y) result correct)) - log 10,
             etType = tSet t ->- tBool }

main = do 
    args@[rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
    putStrLn $ "Number (EC) run with: " ++ unwords args 
    setStdGen $ mkStdGen $ read rndSeed
    let seed = Grammar { grApp = log 0.35,
                         grExprDistr = Map.fromList 
                             [ (annotateRequested e, 1.0) | e <- numberWordExprs ] }
        tasks = [ makeNumberTask thisN | thisN <- [1..10] ]
    good <- loopM seed [0..9] $ \grammar step -> do
        putStrLn $ "EM Iteration: " ++ show step
        grammar' <- doEMIter (prefix++"/best_"++show step) tasks (read lambda) 
          (read pseudocounts) (read fSize) grammar  
        saveGrammar (prefix++"/grammar_" ++ show step) grammar'
        return grammar'
    let ps = enumBits good 7500 (tSet t ->- tBool)
    -- print ps
    forM_ tasks $ \tsk -> do
        let ps' = filter (\p -> not (isInvalidNum $ etLogLikelihood tsk p)) ps
        if null ps'
            then putStrLn $ (etName tsk) ++ " = []"
            else putStrLn $ (etName tsk) ++ " = " ++ 
              show (taskFeatures good (tInt ->- tInt) ps')
        return ()
