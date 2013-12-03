module Main where

import Strappy.BottomUp
import Strappy.Expr
import Strappy.Type
import Strappy.Utils
import Strappy.Library
import Strappy.BeamSearch
import Strappy.Planner
import Strappy.EM
import Strappy.ProdFeat

import qualified Data.Map as Map
import System.Environment
import System.Random
import Data.Maybe
import Control.Monad
import Debug.Trace
import Data.List
import Data.Array

makeWordTask :: String -> Int -> (Type, Expr, String, Int)
makeWordTask str cnt =
    (tList tChar, e str, str, cnt)
    where e [] = cEmpty
          e (c:cs) = (cCons <> cChar2Expr c) <> e cs

main = do
  args@[rndSeed, lambda, pseudocounts, fSize, keepSize, prefix] <- getArgs
  putStrLn $ "Words run with: " ++ unwords args
  let seed = Grammar { grApp = log 0.5,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- wordExprs ] }
  let tasks = [makeWordTask wd 100 | wd <- ["antigen", "antimatter", "antifur", "anticipation", "cation", "diction", "notion" ] ]
  finalG <- loopM seed [0..0] $ \grammar step -> do
    putStrLn ("EM Iteration: " ++ show step)
    grammar' <- doBUIter (prefix++"/best_"++show step) tasks
                         (read lambda) (read pseudocounts) (read fSize) (read keepSize) grammar
    saveGrammar (prefix++"/grammar_" ++ show step) grammar'
    return grammar'
  -- Feature extraction
  putStrLn $ "Final features:\n\n" ++ unlines (featureNames finalG) 
  putStrLn "Doing a final round of enumeration..."
  forM_ tasks $ \(tp, seed, nm, _) -> do
    front <- enumBU (read fSize) (read keepSize) finalG tp seed
    let front' = Map.keys front
    putStrLn $ "Features for " ++ nm
    putStrLn $ unlines $ map show $ taskFeatures finalG tp front'
  return ()

makeWordEMTask :: String -> EMTask
makeWordEMTask wd = EMTask { etName = wd,
                             etLogLikelihood =
                              \e -> let ll = case timeLimitedEval e of
                                               Nothing -> log 0.0
                                               Just s -> - fromIntegral (editDistance wd s)
                                    in ll,
                             etType = tList tChar }


-- Edit distance, taken from http://www.haskell.org/haskellwiki/Edit_distance
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = table ! (m,n)
    where
    (m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)
 
    table :: Array (Int,Int) Int
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))
 
    dist (0,j) = j
    dist (i,0) = i
    dist (i,j) = minimum [table ! (i-1,j) + 1, table ! (i,j-1) + 1,
        if x ! i == y ! j then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]

{-

-- Uses EC for the word task
main = do
  args@[rndSeed, planOrEM, lambda, pseudocounts, fSize, beamSize, planLen, prefix] <- getArgs
  putStrLn $ "Word (EC) run with: " ++ unwords args
  setStdGen $ mkStdGen $ read rndSeed
  let planning = head planOrEM == 'p'
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- wordExprs ] }
  let tasks = [ makeWordEMTask ("anti" ++ [suffix]) | suffix <- "bcde" ]
  let planTasks = map convert2planTask tasks
  loopM seed [0..14] $ \grammar step -> do
    if planning
       then putStrLn $ "EM Planning Iteration: " ++ show step
       else putStrLn $ "EM Iteration: " ++ show step
    grammar' <- if planning
                then liftM fst $ doEMBeam Nothing planTasks (read lambda) (read pseudocounts)
                                          (read fSize) (read beamSize) (read planLen) grammar
                else doEMIter (prefix++"/best_"++show step) tasks
                              (read lambda) (read pseudocounts) (read fSize) grammar
    saveGrammar (prefix++"/grammar_" ++ show step) grammar'
    return grammar'
  return ()
-}