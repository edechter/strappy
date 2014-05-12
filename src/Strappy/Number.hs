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

-- | TASK SPECIFIC UTILITIES | --

mkBagExpr x = intListToExpr $ replicate x 1

compareMaybe :: (Eq a1) => (a -> a1) -> Maybe a -> Maybe a -> Bool
compareMaybe _ _ Nothing = False
compareMaybe _ Nothing _ = False
compareMaybe f (Just x) (Just y) = (f x) == (f y)

-- | TASKS | -- 

makePredXTask :: Int -> EMTask
makePredXTask x = 
    EMTask { etName = "pred_" ++ show x,
             etLogLikelihood =
                 \e -> let result  = timeLimitedEval (e <> (mkBagExpr x)) :: Maybe [Int]
                           correct = Just (replicate (x-1) 1)             :: Maybe [Int]
                       in log . fromIntegral . bool2Binary $ compareMaybe length result correct,
             etType = tList tInt }

makePredTask :: EMTask
makePredTask = 
    EMTask { etName = "pred",
             etLogLikelihood =
                 \e -> let result  = [ timeLimitedEval (e <> (mkBagExpr x)) | x <- [2..10] ] :: [Maybe [Int]]
                           correct = [ Just (replicate (x-1) 1)             | x <- [2..10] ] :: [Maybe [Int]]
                       in (log . fromIntegral . sum $ zipWith (\ x y -> bool2Binary $ compareMaybe length x y) result correct) - (log . fromIntegral $ length result),
             etType = tList tInt ->- tList tInt }

makeSuccXTask :: Int -> EMTask
makeSuccXTask x = 
    EMTask { etName = "succ_" ++ show x,
             etLogLikelihood =
                 \e -> let result  = timeLimitedEval (e <> (mkBagExpr x)) :: Maybe [Int]
                           correct = Just (replicate (x+1) 1)             :: Maybe [Int]
                       in log . fromIntegral . bool2Binary $ compareMaybe length result correct,
             etType = tList tInt }

makeSuccTask :: EMTask
makeSuccTask = 
    EMTask { etName = "succ",
             etLogLikelihood =
                 \e -> let result  = [ timeLimitedEval (e <> (mkBagExpr x)) | x <- [1..9] ] :: [Maybe [Int]]
                           correct = [ Just (replicate (x+1) 1)             | x <- [1..9] ] :: [Maybe [Int]]
                       in (log . fromIntegral . sum $ zipWith (\ x y -> bool2Binary $ compareMaybe length x y) result correct) - (log . fromIntegral $ length result),
             etType = tList tInt ->- tList tInt }

makeSameXTask :: Int -> EMTask
makeSameXTask x = 
    EMTask { etName = "same_" ++ show x,
             etLogLikelihood = -- partial credit: LL is # correct/# total.
                 \e -> let result  = [ timeLimitedEval (e <> (mkBagExpr y)) | y <- [1..10] ] :: [Maybe Bool]
                           correct = [ Just (x == y)                        | y <- [1..10] ] :: [Maybe Bool]
                       in (log . fromIntegral . sum $ zipWith (\ x y -> bool2Binary $ compareMaybe id x y) result correct) - (log . fromIntegral $ length result),
             etType = tList tInt ->- tBool }

makeSameTask :: EMTask
makeSameTask =
    EMTask { etName = "same",
             etLogLikelihood =
                 \e -> let result  = [ timeLimitedEval (e <> (mkBagExpr n1) <> (mkBagExpr n2)) | n1 <- [1..10], n2 <- [1..10] ] :: [Maybe Bool]
                           correct = [ Just (n1 == n2) | n1 <- [1..10], n2 <- [1..10] ] :: [Maybe Bool]
                       in (log . fromIntegral . sum $ zipWith (\ x y -> bool2Binary $ compareMaybe id x y) result correct) - (log . fromIntegral $ length result),
             etType = tList tInt ->- tList tInt ->- tBool }

makeMoreXTask :: Int -> EMTask
makeMoreXTask x = 
    EMTask { etName = "more_" ++ show x,
             etLogLikelihood = -- partial credit: LL is # correct/# total.
                 \e -> let result  = [ timeLimitedEval (e <> (mkBagExpr y)) | y <- [1..10] ] :: [Maybe Bool]
                           correct = [ Just (x < y)                         | y <- [1..10] ] :: [Maybe Bool]
                       in (log . fromIntegral . sum $ zipWith (\ x y -> bool2Binary $ compareMaybe id x y) result correct) - (log . fromIntegral $ length result),
             etType = tList tInt ->- tBool }

makeMoreTask :: EMTask
makeMoreTask =
    EMTask { etName = "more",
             etLogLikelihood =
                 \e -> let result  = [ timeLimitedEval (e <> (mkBagExpr n1) <> (mkBagExpr n2)) | n1 <- [1..10], n2 <- [1..10] ] :: [Maybe Bool]
                           correct = [ Just (n1 < n2)                                          | n1 <- [1..10], n2 <- [1..10] ] :: [Maybe Bool]
                       in (log . fromIntegral . sum $ zipWith (\ x y -> bool2Binary $ compareMaybe id x y) result correct) - (log . fromIntegral $ length result),
             etType = tList tInt ->- tList tInt ->- tBool }

makeLessXTask :: Int -> EMTask
makeLessXTask x = 
    EMTask { etName = "less_" ++ show x,
             etLogLikelihood = -- partial credit: LL is # correct/# total.
                 \e -> let result  = [ timeLimitedEval (e <> (mkBagExpr y)) | y <- [1..10] ] :: [Maybe Bool]
                           correct = [ Just (x > y)                         | y <- [1..10] ] :: [Maybe Bool]
                       in (log . fromIntegral . sum $ zipWith (\ x y -> bool2Binary $ compareMaybe id x y) result correct) - (log . fromIntegral $ length result),
             etType = tList tInt ->- tBool }

makeLessTask :: EMTask
makeLessTask =
    EMTask { etName = "less",
             etLogLikelihood =
                 \e -> let result  = [ timeLimitedEval (e <> (mkBagExpr n1) <> (mkBagExpr n2)) | n1 <- [1..10], n2 <- [1..10] ] :: [Maybe Bool]
                           correct = [ Just (n1 > n2)                                          | n1 <- [1..10], n2 <- [1..10] ] :: [Maybe Bool]
                       in (log . fromIntegral . sum $ zipWith (\ x y -> bool2Binary $ compareMaybe id x y) result correct) - (log . fromIntegral $ length result),
             etType = tList tInt ->- tList tInt ->- tBool }

main = do 
    args@[rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
    putStrLn $ "Number (EC) run with: " ++ unwords args 
    setStdGen $ mkStdGen $ read rndSeed
    let seed = Grammar { grApp = log 0.35,
                         grExprDistr = Map.fromList 
                             [ (annotateRequested e, 1.0) | e <- numberExprs ] }
        tasks = [ makeSameXTask x | x <- [1..10] ] ++ [ makeSameTask ] ++
                [ makeMoreXTask x | x <- [1..10] ] ++ [ makeMoreTask ] ++
                [ makeLessXTask x | x <- [1..10] ] ++ [ makeLessTask ] ++
                [ makeSuccTask, makePredTask ]
    good <- loopM seed [0..9] $ \grammar step -> do
        putStrLn $ "EM Iteration: " ++ show step
        grammar' <- doEMIter (prefix++"/best_"++show step) tasks (read lambda) 
          (read pseudocounts) (read fSize) grammar  
        saveGrammar (prefix++"/grammar_" ++ show step) grammar'
        return grammar'
    putStrLn "Finished with EM Iterations"
