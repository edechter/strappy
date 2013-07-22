-- | This module compresses a set of combinators by solving the corresponding linear program.

module Strappy.LPCompress (compressLP_corpus, compressWeightedCorpus) where

import Strappy.Expr
import Strappy.Library
import Strappy.Utils
import Strappy.Config

import Data.LinearProgram
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans
import Debug.Trace
import System.IO.Unsafe
import System.CPUTime


-- | Wrapper over compressLP_corpus that builds the grammar
compressWeightedCorpus :: Double -> -- ^ lambda
                          Double -> -- ^ pseudocounts
                          Grammar -> -- ^ initial grammar
                          [(Expr, Double)] -> -- ^ weighted corpus
                          Grammar
compressWeightedCorpus lambda pseudocounts grammar corpus =
  let subtrees = foldl1 (Map.unionWith (+)) $ map (countSubtrees Map.empty) corpus
      terminals = filter isTerm $ Map.keys $ grExprDistr grammar
      newProductions = compressCorpus lambda subtrees
      productions = map annotateRequested $ newProductions ++ terminals
      uniformLogProb = -log (genericLength productions)
      grammar'   = Grammar (log 0.5) $ Map.fromList [ (prod, uniformLogProb) | prod <- productions ]
      grammar''  = if pruneGrammar
                   then removeUnusedProductions grammar' $ map fst corpus
                   else grammar'
      grammar''' = inoutEstimateGrammar grammar'' pseudocounts corpus
  in grammar'''

-- Weighted Nevill-Manning
compressCorpus :: Double -> ExprMap Double -> [Expr]
compressCorpus lambda counts =
  map fst $ filter (\(_, c) -> c >= lambda) $ Map.toList counts

-- | Compresses a corpus
compressLP_corpus :: Double -> -- ^ lambda
              ExprMap Double -> -- ^ subtree weights
              [Expr] -- ^ Expressions in the grammar
compressLP_corpus lambda counts = unsafePerformIO $ evalLPT $ do
  start <- lift getCPUTime
  buildLP lambda counts
  (retval, result) <- glpSolve (SimplexOpts MsgOff 1000 True)
  end <- lift getCPUTime
  lift $ putStrLn $ "Solved LP in " ++ show ((fromIntegral $ end-start)/(10^12::Double)) ++ " seconds."
  case result of
    Nothing -> return []
    Just (_, lib) ->
      return $ map (fst . snd) $ filter (\(id, (e, _)) ->
                         case Map.lookup id lib of
                           Nothing -> error "LP bug"
                           Just x | x < 0.01 -> False
                                  | x > 0.99 -> True
                           -- Should never occur if LP relaxation is valid
                           -- If this ever happens, then the math is probably bad
                                  | otherwise -> error $ "LP has coordinate = " ++ show x)
                       $ zip [0..] $ Map.toList counts 


{-buildLP :: Double -> -- ^ lambda
           ExprMap Double -> -- ^ subtree weights
           LPM Int Int () -- ^ LP in a monad-}
buildLP lambda counts = do
  -- Assign ID numbers to all of the subtrees
  let taggedCounts = zip [0..] $ Map.toList counts
  setDirection Min -- We're minimizing description length
  forM_ taggedCounts $ \(tag, (App{ eLeft = l, eRight = r}, w)) -> do
    varBds tag 0 1
    addObjective $ Map.fromList [(tag, lambda-w)]
    -- Add left/right constraints
    case find (\(_, (e, _)) -> e == l) taggedCounts of
      Nothing -> return ()
      Just (lTag, _) -> geqTo (Map.fromList [(lTag, 1), (tag, -1)]) 0
    case find (\(_, (e, _)) -> e == r) taggedCounts of
      Nothing -> return ()
      Just (rTag, _) -> geqTo (Map.fromList [(rTag, 1), (tag, -1)]) 0


-- Test cases
square = (cS <> cI) <> cTimes
p1 = square <> (cInts !! 2)
p2 = square <> (cInts !! 5)
p3 = square <> (cInts !! 8)
corpus = [(p1, 1.0), (p2, 1.0), (p3, 1.0)]
counts = foldl1 (Map.unionWith (+)) $ map (countSubtrees $ Map.empty) corpus

