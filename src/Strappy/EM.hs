
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

-- Evaluates to the number of nats required to encode the grammar
-- Does not include production probabilities TODO: why not?
-- Also assumes uniform production probabilities
-- The justification for this is that the production probabilities in the programs
-- might be very different from the production probabilities in the grammar
-- This isn't a very good solution, and we should do something better.
descriptionLength :: Grammar -> Double
descriptionLength gr@(Grammar{grApp=app, grExprDistr=lib}) =
  Map.foldWithKey (\ k _ s -> s + productionLenTopLevel (fromUExpr k)) 0.0 lib
  where 
    productionLenTopLevel :: Expr a -> Double
    productionLenTopLevel App{eLeft=l, eRight=r} =
      productionLen l + productionLen r 
    productionLenTopLevel Term{} = 0
    productionLen :: Expr a -> Double
    productionLen a@App{eLeft=l, eRight=r, eReqType = Just tp} =
      if Map.member (toUExpr a) lib
      then logSumExp (log 2 + log (numDistractors tp) - log (1 - exp app))
                     (log 2 + productionLen l + productionLen r)
      else log 2 + productionLen l + productionLen r
    productionLen t@Term{eReqType = Just tp}
      = log (numDistractors tp) - log (1 - exp app)
    numDistractors tp = 
      foldl
      (\acc tp' -> if canUnify tp tp'
                   then acc+1.0
                   else acc)
      0.0 libTypes
    libTypes = map (eType . fromUExpr) (Map.keys lib)

-- Likelihood term in EM
grammarLogLikelihood :: Grammar -> [(UExpr, Double)] -> Double
grammarLogLikelihood gr exprs_and_score =
  sum $ map (\(e, w) -> exprLogLikelihood gr (fromUExpr e) * w) exprs_and_score


-- Performs hill climbing upon the given grammar
optimizeGrammar :: Double -> -- ^ Lambda
                   Double -> -- ^ pseudocounts
                   Grammar -> [(UExpr, Double)] -> Grammar
optimizeGrammar lambda pseudocounts gr exprs_and_scores=
  let gr' = estimateGrammar gr 1.0 exprs_and_scores
      -- TODO: BIC or something like that for continuous params
      initial_score = lambda * (descriptionLength gr') - grammarLogLikelihood gr' exprs_and_scores
      -- This procedure performs hill climbing
      climb :: Grammar -> Double -> Grammar
      climb g g_score =
        let gs = grammarNeighbors g pseudocounts exprs_and_scores
            gsScore = [ (g, lambda * descriptionLength g - grammarLogLikelihood g exprs_and_scores) |
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
                -> [(UExpr, Double)] 
                ->  [Grammar]
grammarNeighbors (Grammar appProb lib) pseudocounts obs = oneRemoved ++ oneAdded
  where oneRemoved = map (\expr -> Grammar appProb (Map.delete expr lib)) 
                         $ filter (not . isTerm . fromUExpr) $ Map.keys lib 
        oneAdded =
          let duplicatedSubtrees =
                foldl (\cnt expr_wt -> countSubtreesNotInGrammar lib cnt (first fromUExpr expr_wt)) Map.empty obs 
              subtreeSizes = Map.mapWithKey (\uexpr cnt -> cnt * exprSize lib (fromUExpr uexpr)) duplicatedSubtrees
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
                            -> (Expr a, Double) -- <expr>: the expression
                            -> ExprMap Double
countSubtreesNotInGrammar lib cnt (expr@(App{eLeft=l, eRight=r}),wt) =
  let cnt'   = incCountIfNotInGrammar lib cnt (expr,wt)
      cnt''  = countSubtreesNotInGrammar lib cnt' (l,wt)
      cnt''' = countSubtreesNotInGrammar lib cnt'' (r,wt)
  in
   cnt'''
countSubtreesNotInGrammar _ cnt _ = cnt

incCountIfNotInGrammar :: ExprDistr -> ExprMap Double -> (Expr a,Double) 
                          -> ExprMap Double
incCountIfNotInGrammar lib cnt (expr,wt) | not (Map.member (toUExpr expr) lib) =
  Map.insertWith (+) (toUExpr expr) wt cnt
incCountIfNotInGrammar _ cnt _ = cnt

exprSize :: ExprDistr -> Expr a -> Double
exprSize distr e | Map.member (toUExpr e) distr = 1.0
exprSize distr App{eLeft=l, eRight=r} = 1.0 + exprSize distr l + exprSize distr r
exprSize _ _ = 1.0


-- | Performs one iterations of EM on multitask learning
{-doEMIter :: (MonadRandom m, MonadPlus m) =>
            [(UExpr -> Double, Type)] -- ^ Tasks
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Grammar -- ^ Initial grammar
            -> m Grammar -- ^ Improved grammar-}
doEMIter tasks lambda pseudocounts frontierSize grammar = do
  -- For each type, sample a frontier
  frontiers <- mapM (\tp -> do sample <- sampleExprs frontierSize grammar tp
                               return (tp, sample))
                    $ nub $ map snd tasks
  putStrLn "Sampled frontiers."
  -- For each task, weight the corresponding frontier by likelihood
  let weightedFrontiers = Prelude.flip map tasks $ \(tsk, tp) ->
        let frontier = fromJust (lookup tp frontiers)
        in Map.mapWithKey (\expr cnt -> fromIntegral cnt * tsk expr) frontier
  -- Normalize frontiers
  let zs = map (Map.fold (+) 0.0) weightedFrontiers
  let numHit = length $ filter (>0.0) zs
  putStrLn $ "Hit " ++ show numHit ++ " tasks."
  let obs = foldl (\acc (z, frontier) ->
                    if z > 0.0
                    then Map.unionWith (+) acc $ Map.map (/z) frontier
                    else acc) Map.empty $ zip zs weightedFrontiers
  if Map.size obs == 0
    then return grammar -- Didn't hit any tasks
    else do let grammar' = optimizeGrammar lambda pseudocounts grammar $ Map.toList obs
            putStrLn $ "Before InOut: " ++ showGrammar grammar'
            start <- getCPUTime
            let grammar'' = iterateInOut 5 (clearGrammarProbs grammar') 0.3 $ Map.toList obs
            putStrLn $ "After InOut: " ++ showGrammar grammar''
            done <- getCPUTime
            putStrLn $ "InOut took " ++ show (fromIntegral (done-start) / (10^12)) ++ " seconds."
            return grammar''
         


-- Library for testing EM+polynomial regressionx
polyExprs :: [UExpr]
polyExprs = [toUExpr cI, 
              toUExpr cS, 
              toUExpr cB, 
              toUExpr cC, 
              toUExpr cK, 
              toUExpr cPlus,
              toUExpr cTimes
              ] ++ map toUExpr cInts


-- Polynomial regression test for EM
polyEM :: IO ()
polyEM = do
  -- Seed grammar
  let seed = Grammar { grApp = log 0.35,
                       grExprDistr = Map.fromList [ (annotateRequested e, 1.0) | e <- polyExprs ] }
  -- Make nth order polynomial task
  let mkNthOrder n = do
        coeffs <- replicateM (n+1) $ randomRIO (0,9::Int)
        let poly x = sum $ zipWith (*) coeffs $ map (x^) [0..]
        let score proc =
              if map (eval (fromUExpr proc)) [0..3] == map poly [0..3]
              then 1.0
              else 0.0
        return (score, tInt ->- tInt)
  const <- replicateM 100 (mkNthOrder 0)
  lin  <- replicateM 100 (mkNthOrder 1)
  quad <- replicateM 100 (mkNthOrder 2)
  cub  <- replicateM 100 (mkNthOrder 3)
  loopM seed [1..20] $ \grammar step -> do
    putStrLn $ "EM Iteration: " ++ show step
    grammar' <- doEMIter (const++lin++quad++cub) 1.0 0.5 1000 grammar
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