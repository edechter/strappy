
module Main where
--module Strappy.EM where

import Strappy.Sample
import Strappy.Expr
import Strappy.Library
import Strappy.Type
import Strappy.Task
import Strappy.Utils
import Strappy.Library 

import Data.Maybe
import Data.List
import Data.Function
import Control.Arrow (first)
import qualified Data.HashMap as Map
import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Debug.Trace 

import System.CPUTime

-- Negative Log Prior
grammarNLP :: Grammar -> Double
grammarNLP gr@(Grammar{grExprDistr = lib}) =
  let corpus  = filter (not . isTerm) $ Map.keys lib
      destructureAndWeight (App{eLeft = l, eRight = r}) =
        [(l,1.0), (r,1.0)]
      corpus' = concatMap destructureAndWeight corpus
      gr' = iterateInOut 5 (clearGrammarProbs gr) 0.0 corpus'
  in - grammarLogLikelihood gr' corpus'


-- Likelihood term in EM
grammarLogLikelihood :: Grammar -> [(Expr, Double)] -> Double
grammarLogLikelihood gr exprs_and_score =
  sum $ map (\(e, w) -> exprLogLikelihood gr e * w) exprs_and_score


-- Performs hill climbing upon the given grammar
optimizeGrammar :: Double -> -- ^ Lambda
                   Double -> -- ^ pseudocounts
                   Grammar -> [(Expr, Double)] -> Grammar
optimizeGrammar lambda pseudocounts gr exprs_and_scores=
  let gr' = estimateGrammar gr 1.0 exprs_and_scores
      initial_score = lambda * (grammarNLP gr') - grammarLogLikelihood gr' exprs_and_scores
      -- This procedure performs hill climbing
      climb :: Grammar -> Double -> Grammar
      climb g g_score =
        let gs = grammarNeighbors g pseudocounts exprs_and_scores
            gsScore = [ (g, lambda * grammarNLP g - grammarLogLikelihood g exprs_and_scores) |
                        g <- gs ]
            (best_g', best_g'_score) = minimumBy (compare `on` snd) gsScore
        in
         if best_g'_score < g_score
         then climb best_g' best_g'_score
         else g
  in
   climb gr' initial_score

-- | The neighbors of a grammar are those reachable by one addition/removal of a production
-- This procedure tries adding/removing one production,
-- modulo the restriction that the primitives are never removed from the grammar
grammarNeighbors :: Grammar -> Double -- ^ pseudocounts 
                -> [(Expr, Double)] 
                -> [Grammar]
grammarNeighbors (Grammar appProb lib) pseudocounts obs = oneRemoved ++ oneAdded
  where oneRemoved = map (\expr -> Grammar appProb (Map.delete expr lib)) 
                         $ filter (not . isTerm) $ Map.keys lib 
        oneAdded =
          let duplicatedSubtrees =
                foldl (\cnt expr_wt -> countSubtreesNotInGrammar lib cnt expr_wt) Map.empty obs 
              subtreeSizes = Map.mapWithKey (\expr cnt -> cnt * exprSize lib expr) duplicatedSubtrees
              maximumAdded = 5 -- Consider at most 5 subtrees. Cuts down search.
              bestSubtrees = map annotateRequested $
                             take maximumAdded $
                             map fst $
                             sortBy (\a a' -> flipOrdering $ (compare `on` snd) a a') $
                             Map.toList subtreeSizes
          in
           [ estimateGrammar (Grammar appProb (Map.insert subtree 0.0 lib)) pseudocounts obs |
             subtree <- bestSubtrees ]

countSubtreesNotInGrammar :: ExprDistr -- <lib>: Distribution over expressions. 
                            -> ExprMap Double -- <cnt>: Map of rules and associated counts.  
                            -> (Expr, Double) -- <expr>: the expression
                            -> ExprMap Double
countSubtreesNotInGrammar lib cnt (expr@(App{eLeft=l, eRight=r}),wt) =
  let cnt'   = incCountIfNotInGrammar lib cnt (expr,wt)
      cnt''  = countSubtreesNotInGrammar lib cnt' (l,wt)
      cnt''' = countSubtreesNotInGrammar lib cnt'' (r,wt)
  in
   cnt'''
countSubtreesNotInGrammar _ cnt _ = cnt

incCountIfNotInGrammar :: ExprDistr -> ExprMap Double -> (Expr, Double) 
                          -> ExprMap Double
incCountIfNotInGrammar lib cnt (expr,wt) | not (Map.member expr lib) =
  Map.insertWith (+) expr wt cnt
incCountIfNotInGrammar _ cnt _ = cnt

exprSize :: ExprDistr -> Expr -> Double
exprSize distr e | Map.member e distr = 1.0
exprSize distr App{eLeft=l, eRight=r} = 1.0 + exprSize distr l + exprSize distr r
exprSize _ _ = 1.0


-- | Performs one iterations of EM on multitask learning
doEMIter :: 
            [(Expr -> Double, Type)] -- ^ Tasks
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar-}
doEMIter tasks lambda pseudocounts frontierSize grammar = do
  -- For each type, sample a frontier
  frontiers <- mapM (\tp -> do sample <- return $ sampleBF frontierSize grammar tp
                               return (tp, sample))
                    $ nub $ map snd tasks
  -- For each task, weight the corresponding frontier by likelihood
  let weightedFrontiers = Prelude.flip map tasks $ \(tsk, tp) ->
        Map.mapWithKey (\expr cnt -> cnt * tsk expr) $ fromJust $ lookup tp frontiers
  -- Normalize frontiers
  let zs = map (Map.fold (+) 0.0) weightedFrontiers
  let numHit = length $ filter (>0.0) zs
  putStrLn $ "Hit " ++ show numHit ++ "/" ++ show (length tasks) ++ " tasks."
  let obs = foldl (\acc (z, frontier) ->
                    if z > 0.0
                    then Map.unionWith (+) acc $ Map.map (/z) frontier
                    else acc) Map.empty $ zip zs weightedFrontiers
  if Map.size obs == 0
    then return grammar -- Didn't hit any tasks
    else do let grammar' = optimizeGrammar lambda pseudocounts grammar $ Map.toList obs
            let grammar'' = iterateInOut 5 (clearGrammarProbs grammar') 0.3 $ Map.toList obs
            putStrLn $ showGrammar grammar''
            return grammar''
         
-- Library for testing EM+polynomial regressionx
polyExprs :: [Expr]
polyExprs = [cI, 
              cS, 
              cB, 
              cC, 
              cK, 
              cPlus,
              cTimes
              ] ++ cInts


-- Polynomial regression test for EM
polyEM :: IO ()
polyEM = do
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- polyExprs ] }
  -- Make nth order polynomial task with random coefficients
{-  let mkNthOrder :: Int -> IO (Expr -> Double, Type)
      mkNthOrder n = replicateM (n+1) $ randomRIO (0,9::Int) >>= \coeffs ->
        let poly :: Int -> Int
            poly x = sum $ zipWith (*) coeffs $ map (x^) [0..]
            score proc =
              if map (eval proc) [0..3] == map poly [0..3]
              then 1.0
              else 0.0
        in return (score, tInt ->- tInt)
-}
  -- Make nth order polynomial task with fixed coefficients
  let mkNthDet :: [Int] -> (Expr -> Double, Type)
      mkNthDet coeffs = let poly :: Int -> Int
                            poly x = sum $ zipWith (*) coeffs $ map (x^) [0..]
                            score proc = if map (eval proc) [0..3] == map poly [0..3]
                                         then 1.0
                                         else 0.0
                        in (score, tInt ->- tInt)
  let const = [ mkNthDet [x] | x <- [1..9] ]
  let lin = [ mkNthDet [x,y] | x <- [1..9], y <- [1..9] ]
--  quad <- replicateM 100 (mkNthOrder 2)
  loopM seed [1..100] $ \grammar step -> do
    putStrLn $ "EM Iteration: " ++ show step
    grammar' <- doEMIter (const++lin) 0.3 1.0 10000 grammar
    return grammar'
  return ()
                    
main = polyEM

----------------------------------------------------------------------
-- Solve a single task

-- solveTask :: (MonadPlus m, MonadRandom m) => 
--         -> Int -- ^ <n> plan length
--         -> Int -- ^ <m> branching factor 
--         -> m ( [(UExpr, Double)], -- all expressions tried and associated rewards
--                [UExpr], -- plan of expressions
--                Double) -- score of plan
{-solveTask gr Task{task=tsk, taskType=tp} n m = go [] [] 0 n 
       where  go exprs_and_scores plan cum_score 0 = return (exprs_and_scores, plan, cum_score)
              go exprs_and_scores plan cum_score steps = 
                 let tp' = if steps == n then tp else tp ->- tp 
                 in do exprs <- sequence $ sampleExprs m gr tp' 
                       -- for each expr, append that expression to the plan, 
                       -- score it on the task, and compare the new score to the
                       -- score of the current plan. 
                       
                       let exprs_and_scores'  
                             = do expr <- exprs 
                                  let score = if steps == n then tsk (toUExpr expr) 
                                                else (tsk (compose (toUExpr expr : plan))) / cum_score
                                  return (toUExpr expr, score) 
                           plan' = best_expr : plan                                     
                                   where best_expr = fst $ maximumBy (compare `on` snd) exprs_and_scores' 
                           cum_score' = tsk $ compose plan'            
                       go (exprs_and_scores' ++ exprs_and_scores) plan' cum_score' (steps - 1)

solveTasks
  :: (MonadPlus m, MonadRandom m) =>
     Grammar
     -> [Task]
     -> Int
     -> Int
     -> m ([[(UExpr, Double)]], [[UExpr]], [Double])
solveTasks gr tasks n m = liftM unzip3 $ mapM (\t -> solveTask gr t n m) tasks



-}