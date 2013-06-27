-- | This module compresses a set of combinators by solving the corresponding linear program.

module Strappy.LPCompress (compressLP_corpus, compressLP_EC) where

import Strappy.Expr
import Strappy.Library
import Strappy.Utils

import Data.LinearProgram
import Data.List
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans
import Debug.Trace
import System.IO.Unsafe
import System.CPUTime

-- | Finds most compressive grammar and program assignments for EC
compressLP_EC :: [[Expr]] -> -- ^ List of programs hitting each task
                 ([Expr], -- ^ Expressions in the grammar
                  [Expr]) -- ^ solution set
compressLP_EC hits = unsafePerformIO $ evalLPT $ do
  let numProgs = maximum $ map length hits
  start <- lift getCPUTime
  labeledSubtrees <- buildLP_EC hits
  (retval, result) <- quickSolveLP
  end <- lift getCPUTime
  lift $ putStrLn $ "Solved LP in " ++ show ((fromIntegral $ end-start)/(10^12::Double)) ++ " seconds."
  let minLabel = minimum $ map snd labeledSubtrees
  let maxLabel = maximum $ map snd labeledSubtrees
  let productions = case result of
        Nothing -> []
        Just (_, solution) ->
          map fst $ filter (\(tree, label) ->
                             case Map.lookup label solution of
                               Nothing -> error "LP bug"
                               Just x | x < 0.01 -> False
                                      | x > 0.99 -> True
                                      -- Should never occur if LP relaxation is valid
                                      -- If this ever happens, then the math is probably bad
                                      | otherwise -> error $ "LP has coordinate = " ++ show x)
                           labeledSubtrees
  let solutionSet = case result of
        Nothing -> []
        Just (_, solution) ->
          Prelude.flip map [0 .. (length hits - 1)] $ \i ->
          let kMax = length $ hits!!i
              eiks = map (safeFromJust "e_i^k not in LP solution") $ map (Prelude.flip Map.lookup solution) $
                                                                     [ k + i * numProgs | k <- [0..kMax-1] ]
              chosenK = snd $ maximum $ zip eiks [0..]
          in trace (show eiks) $ (hits!!i)!!chosenK
  return (productions, solutionSet)


buildLP_EC hits = do
  let numTasks = length hits
  let numProgs = maximum $ map length hits
  -- For each task, declare a variable for each possible program.
  -- Demand that at most one program is selected per task.
  forM_ [0..numTasks-1] $ \i -> do
    let numSolutions = length $ hits !! i
    forM_ [0..numSolutions-1] $ \k -> varBds (k + i*numProgs) 0 1
    forM_ [0..numSolutions-1] $ \k -> forM [0 .. numSolutions-1] $ \k' -> when (k /= k') $
      leqTo (Map.fromList [(k + i * numProgs, 1), (k' + i*numProgs, 1)]) 1
  -- Find all subtrees and introduce LP variables for them.
  let subtrees = Map.keys $ foldl (\acc expr -> countSubtrees acc (expr, 1.0)) Map.empty $ concat hits
  let labeledSubtrees = zipWith (\subtree j -> (subtree, j + numTasks * numProgs)) subtrees [0..]
  forM_ labeledSubtrees $ \(_, label) -> varBds label 0 1
  forM_ [0..numTasks-1] $ \i -> do
    let numSolutions = length $ hits !! i
    forM_ [0..numSolutions-1] $ \k -> do
      let eik = k + i * numProgs
      let prog = (hits!!i)!!k
      boundSubtrees labeledSubtrees eik prog
  setDirection Min -- minimize number of unique subtrees
  -- In order to prevent us from selecting none of the programs for a task,
  -- we add a large incentive to selecting any program.
  -- This allows us to preserve total unimodularity while still solving the right ILP via relaxation.
  let useProgramReward = 2 * length subtrees -- At worst, a program costs us every subtree
  setObjective $ Map.fromList $ [(label, 1::Int) | (_, label) <- labeledSubtrees ] ++
                                [ (k + i * numProgs, -useProgramReward) |
                                  i <- [0..numTasks-1], k <- [0.. ((length $ hits!!i)-1) ] ]
  return labeledSubtrees
  where boundSubtrees subs eik (Term {}) = return ()
        boundSubtrees subs eik e@(App { eLeft = l, eRight = r}) = do
          case lookup e subs of
            Nothing -> error "Could not find subtree in LP"
            Just label -> do geqTo (Map.fromList [(label, 1), (eik, -1)]) 0
                             boundSubtrees subs eik l
                             boundSubtrees subs eik r
    

-- | Wrapper over compressLP_corpus that builds the grammar for you
compressWeightedCorpus :: Double -> -- ^ lambda
                          Double -> -- ^ pseudocounts
                          Grammar -> -- ^ initial grammar
                          [(Expr, Double)] -> -- ^ weighted corpus
                          Grammar
compressWeightedCorpus lambda pseudocounts grammar corpus =
  let subtrees = foldl1 (Map.unionWith (+)) $ map (countSubtrees Map.empty) corpus
      terminals = filter isTerm $ Map.keys $ grExprDistr grammar
      newProductions = compressLP_corpus lambda subtrees
      productions = newProductions ++ terminals
      uniformLogProb = -log (genericLength productions)
      grammar'   = Grammar (log 0.5) $ Map.fromList [ (prod, uniformLogProb) | prod <- productions ]
      grammar''  = if pruneGrammar
                   then removeUnusedProductions grammar' $ map fst corpus
                   else grammar'
      grammar''' = inoutEstimateGrammar grammar'' pseudocounts corpus
  in grammar'''

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

