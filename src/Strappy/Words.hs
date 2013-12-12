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
import Strappy.SymDimRed

import qualified Data.Map as Map
import System.Environment
import System.Random
import Data.Maybe
import Control.Monad
import Debug.Trace
import Data.List
import Data.Array

main = mainSymRed

makeWordTask :: String -> (Type, Expr, String)
makeWordTask str =
    (tList tChar, e str, str)
    where e [] = cEmpty
          e (c:cs) = (cCons <> cChar2Expr c) <> e cs

-- | Slurps in a grammar and spits out a log likelihood of held-out test data
mainLL = do
  [gFile] <- getArgs
  g <- loadGrammar gFile
  let heldout = [ "antiparticle", "antichrist", "antithetical",
                   "antiseptic", "antiquark", "antineutrino", "antiproton", "antielectron",
                   "anticipation",
                   "convolution", "compilation", "dereliction", "fiction",
                  "revolution", "caution", "motion"]
  lls <- forM heldout $ \wd -> do
    let (_, wd', _) = makeWordTask wd
    solns <- enumBU 15000 15000 g (tList tChar) wd'
    return $ logSumExpList $ map (fromJust . eLogLikelihood . fst) $ Map.toList solns
  putStrLn $ show $ sum lls

testLL g = do
  let heldout = [ "antiparticle", "antichrist", "antithetical",
                   "antiseptic", "antiquark", "antineutrino", "antiproton", "antielectron",
                   "anticipation",
                   "convolution", "compilation", "dereliction", "fiction",
                  "revolution", "caution", "motion"]
  lls <- forM heldout $ \wd -> do
    let (_, wd', _) = makeWordTask wd
    solns <- enumBU 15000 15000 g (tList tChar) wd'
    return $ logSumExpList $ map (fromJust . eLogLikelihood . fst) $ Map.toList solns
  putStrLn $ show $ sum lls

mainFeat = do
  [gFile] <- getArgs
  g <- loadGrammar gFile
  putStrLn $ "loaded " ++ gFile
  let tot = [ "antiparticle", "antichrist", "antithetical",
                   "antiseptic", "antiquark", "antineutrino", "antiproton", "antielectron",
                   "antifur", "antimatter", "antigen",
                   "anticipation",
                   "convolution", "compilation", "dereliction", "fiction",
                  "revolution", "caution", "motion",
                  "notion", "cation", "diction"]
  let tasks = [makeWordTask wd | wd <- tot ]
  forM_ tasks $ \(tp, seed, nm) -> do
    putStrLn $ "enuming for" ++ nm
    front <- enumBU 15000 500 g tp seed
    putStrLn "done"
    let front' = Map.keys front
    putStrLn $ nm ++ "=" ++ show (taskFeatures g tp front')

mainSymRed = do
  args@[fSize, keepSize, gNm] <- getArgs
  let g0   = Grammar { grApp = log 0.5,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- wordExprs ] }
  g <- if gNm == "g0"
       then return g0
       else loadGrammar gNm
  let tasks = [makeWordTask wd | wd <- ["notion", "cation", "antifur", "antimatter", "antigen"]]
  putStrLn "Doing a symbolic dimensionality reduction..."
  fs <- forM tasks $ \(tp, seed, nm) -> do
    front <- enumBU 15000 500 g tp seed
    return $ Map.keys front
  putStrLn $ show $ mlDecoder g0 (tList tChar) fs

mainBU = do
  args@[rndSeed, lambda, pseudocounts, fSize, keepSize, prefix] <- getArgs
  putStrLn $ "Words run with: " ++ unwords args
  let seed = Grammar { grApp = log 0.5,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- wordExprs ] }
  let tasks = [makeWordTask wd | wd <- ["antifur", "antimatter", "antigen",
--                                            "anticipation",
                                            "notion", "cation", "diction"] ]
  finalG <- loopM seed [0..1] $ \grammar step -> do
    putStrLn ("EM Iteration: " ++ show step)
    grammar' <- doBUIter (prefix++"/best_"++show step) tasks
                         (read lambda) (read pseudocounts) (read fSize) (read keepSize) grammar
    saveGrammar (prefix++"/grammar_" ++ show step) grammar'
    return grammar'
  testLL finalG
  -- Feature extraction
  {-putStrLn $ "Final features:\n\n" ++ unlines (featureNames finalG) 
  putStrLn "Doing a final round of enumeration..."
  forM_ tasks $ \(tp, seed, nm) -> do
    front <- enumBU (read fSize) (read keepSize) finalG tp seed
    let front' = Map.keys front
    putStrLn $ "Features for " ++ nm
    putStrLn $ unlines $ map show $ taskFeatures finalG tp front'-}
  {-putStrLn "Doing a symbolic dimensionality reduction..."
  fs <- forM tasks $ \(tp, seed, nm, _) -> do
    front <- enumBU (read fSize) (read keepSize) seed tp seed
    return $ Map.keys front
  putStrLn $ show $ mlDecoder seed (tList tChar) fs-}
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



-- Uses EC for the word task
mainEC = do
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