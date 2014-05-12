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

mkBagExpr x = intListToExpr $ replicate x 1

compareMaybe :: (Eq a1) => (a -> a1) -> Maybe a -> Maybe a -> Bool
compareMaybe _ _ Nothing = False
compareMaybe _ Nothing _ = False
compareMaybe f (Just x) (Just y) = (f x) == (f y)

makeNumberTask :: Int -> EMTask
makeNumberTask n1 = 
    EMTask { etName = "num_" ++ show n1,
             etLogLikelihood = -- partial credit: LL is # correct/# total.
                 \e -> let result  = [ timeLimitedEval (e <> (mkBagExpr n)) | n <- [1..10] ] :: [Maybe Bool]
                           correct = (map (\x -> Just (x == n1)) [1..10]) :: [Maybe Bool]
                       in log (fromIntegral (sum $ zipWith (\x y -> bool2Binary $ x == y) result correct)) - log 10,
             etType = tList tInt ->- tBool }

makeEqualTask :: Int -> Int -> EMTask
makeEqualTask n1 n2 = -- are two sets of equal size?
    EMTask { etName = "equal_" ++ show n1 ++ "_" ++ show n2
             etLogLikelihood =
                 \e -> let result = timeLimitedEval (e <> (mkBagExpr n1) <> (mkBagExpr n2)) :: Maybe Bool
                           correct = Just (n1 == n2) :: Maybe Bool
                       in log . fromIntegral . bool2Binary $ compareMaybe id result correct,
             etType = tList tInt ->- tList tInt ->- tBool }

makePredTask :: Int -> EMTask
makePredTask n1 = 
    EMTask { etName = "pred_" ++ show n1,
             etLogLikelihood =
                 \e -> let result  = timeLimitedEval (e <> (mkBagExpr n1)) :: Maybe [Int]
                           correct = Just $ replicate (n1-1) 1 :: Maybe [Int]
                       in log . fromIntegral . bool2Binary $ compareMaybe length result correct,
             etType = tList tInt ->- tList tInt }

makeSuccTask :: Int -> EMTask
makeSuccTask n1 = 
    EMTask { etName = "succ_" ++ show n1,
             etLogLikelihood =
                 \e -> let result  = timeLimitedEval (e <> (mkBagExpr n1)) :: Maybe [Int]
                           correct = Just $ replicate (n1+1) 1 :: Maybe [Int]
                       in log . fromIntegral . bool2Binary $ compareMaybe length result correct,
             etType = tList tInt ->- tList tInt }

main = do 
    args@[rndSeed, lambda, pseudocounts, fSize, prefix] <- getArgs
    putStrLn $ "Number (EC) run with: " ++ unwords args 
    setStdGen $ mkStdGen $ read rndSeed
    let seed = Grammar { grApp = log 0.35,
                         grExprDistr = Map.fromList 
                             [ (annotateRequested e, 1.0) | e <- numberWordExprs ] }
        tasks = [ makeNumberTask n1    | n1 <- [1..10]                ] ++
                [ makeSuccTask   n1    | n1 <- [1..9]                 ] ++
                [ makePredTask   n1    | n1 <- [2..10]                ] ++
                [ makeEqualTask  n1 n2 | n1 <- [1..10], n2 <- [1..10] ]
    good <- loopM seed [0..9] $ \grammar step -> do
        putStrLn $ "EM Iteration: " ++ show step
        grammar' <- doEMIter (prefix++"/best_"++show step) tasks (read lambda) 
          (read pseudocounts) (read fSize) grammar  
        saveGrammar (prefix++"/grammar_" ++ show step) grammar'
        return grammar'
    putStrLn "Finished with EM Iterations"
