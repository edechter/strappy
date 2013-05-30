
module Strappy.EM where

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

-- Evaluates to the number of nats required to encode the grammar
-- Does not include production probabilities TODO: why not?
descriptionLength :: Grammar -> Double
descriptionLength gr@(Grammar{grApp=app, grExprDistr=lib}) =
  Map.foldWithKey (\ k _ s -> s + productionLenTopLevel (fromUExpr k)) 0.0 lib
  where 
    productionLenTopLevel :: Expr a -> Double
    productionLenTopLevel a@App{eLeft=l, eRight=r} 
        = case Map.lookup (toUExpr a) lib of
              Nothing -> error "descriptionLength::productionLenTopLevel: labelled application is not in library."
              (Just ll) -> negate ll + app + productionLen l + productionLen r 
    productionLenTopLevel t@Term{}
        = case Map.lookup (toUExpr t) lib of
              Nothing -> error "descriptionLength::productionLenTopLevel: labelled application is not in library."
              (Just ll) -> negate ll
    productionLen :: Expr a -> Double
    productionLen a@App{eLeft=l, eRight=r} | isLabeled a = case Map.lookup (toUExpr a) lib of 
                                               Nothing -> error "descriptionLength::productionLen: labelled application is not in library."
                                               (Just ll) -> negate ll
                                           | otherwise  = app + productionLen l + productionLen r
    productionLen t@Term{}
        = case Map.lookup (toUExpr t) lib of
              Nothing -> error "descriptionLength::productionLen: labelled application is not in library."
              (Just ll) -> negate ll

-- Likelihood term in EM
-- (frontier!!i)!!k   ==   <e_i^k, P(e_i^k | t_i, G^old)>
-- The probabilities should be normalized, eg,
--    forall i.  (sum $ map snd $ frontier!!i) == 1
-- grammarLogLikelihood :: Grammar -> [[(UExpr,Double)]] -> Double
-- grammarLogLikelihood gr frontier =
--   -- Calculate prob of new grammar generating each combinator
--   let combLLs = map (map (\ (e,_) -> exprLogLikelihood gr (fromUExpr e))) frontier
--       wMatrix = map (map snd) frontier
--   in sum $ zipWith (\lls ws -> sum $ zipWith (*) lls ws) combLLs wMatrix

grammarLogLikelihood gr exprs_and_score = sum $ zipWith (\e w -> exprLogLikelihood gr (fromUExpr e) * w) exprs scores where (exprs, scores) = unzip exprs_and_score
        

-- Returns a given number of samples from the grammar
--sampleFrontier :: MonadRandom m =>
--                  Grammar -> Int -> Type -> m [UExpr]
--sampleFrontier gr size tp =
--  evalStateT (sample' size) 0
--  where sample' 0 = return []
--        sample' n = do
--          maybeSample <- maybeSampleFromGrammar gr ty
--          case maybeSample of
--            Nothing -> sample' n
--            Just s -> liftM (s:) $ sample' (n-1)
  
-- 
--
-- Takes a grammar and a list of tasks; updates to a new grammar
-- doEMIteration :: (MonadPlus m, MonadRandom m )=>
--                  Double
--                  -> Grammar 
--                  -> [UExpr] 
--                  -> Int 
--                  -> [Task] -> m Grammar 
-- doEMIteration lambda gr primitives size tasks = do
--   let ms = map (sampleExprs size gr . taskType) tasks 
--   xs <- sequence . map sequence $ ms 
--   let frontiers = map (map toUExpr) xs :: [[UExpr]]
--   -- Weight each frontier by likelihood
--   let frontiers' = zipWith (\frontier Task{task=tsk} ->
--                              map (\ expr -> (expr, tsk expr))
--                              frontier)
--                    frontiers tasks
--   -- Compute normalizing constants
--   let zs = map (sum . map snd) frontiers'
--   -- Remove unhit tasks
--   let z_and_frontier = filter ((>0) . fst) $ zip zs frontiers'
--   -- Divide by normalizing constant
--   let frontiers'' = map (\(z, frontier) ->
--                           map (\ (expr, posterior) -> (expr, posterior/z))
--                           frontier)
--                     z_and_frontier
--   return $ optimizeGrammar lambda gr primitives frontiers''
-- 
-- Performs hill climbing upon the given grammar
optimizeGrammar :: Double -> Grammar -> [UExpr]

                   -> [(UExpr, Double)] -> Grammar
optimizeGrammar lambda gr primitives exprs_and_scores=
  let gr' = estimateGrammar gr 1.0 exprs_and_scores
      -- TODO: BIC or something like that for continuous params
      initial_score = lambda * (descriptionLength gr') - grammarLogLikelihood gr' exprs_and_scores
      -- This procedure performs hill climbing
      climb :: Grammar -> Double -> Grammar
      climb g g_score =
        let gs = grammarNeighbors g primitives 1.0 exprs_and_scores
            gsScore = [ (g, lambda * descriptionLength g - grammarLogLikelihood g (relabelFrontier g exprs_and_scores)) |
                        g <- gs ]
            (best_g', best_g'_score) = trace ("\n\nGrScores: " ++ concat [showGrammar g ++ "\n\n" ++ show d ++ "\n\n"| (g, d) <- gsScore] ) $ minimumBy (compare `on` snd) gsScore
        in
         if best_g'_score < g_score

         then climb best_g' best_g'_score
         else trace ("Best scores: " ++ best_g'_score ++ " " ++ g_score ++ "\n\n") g
  in
   climb gr' initial_score

-- | The neighbors of a grammar are those reachable by one addition/removal of a production
-- This procedure tries adding/removing one production,
-- modulo the restriction that the primitives are never removed from the grammar
grammarNeighbors :: Grammar -> [UExpr] -> Double -- ^ pseudocounts 
                -> [(UExpr, Double)] 
                ->  [Grammar]
grammarNeighbors (Grammar appProb lib) primitives pseudocounts obs = oneRemoved ++ oneAdded
  where oneRemoved = map (\expr -> Grammar appProb (Map.delete expr lib)) 
                         $ Map.keys lib \\ primitives
        oneAdded =
          let duplicatedSubtrees =
                foldl (\cnt expr_wt -> countSubtreesNotInGrammar lib cnt (first fromUExpr expr_wt)) Map.empty obs 
              subtreeSizes = Map.mapWithKey (\uexpr cnt -> cnt * exprSize lib (fromUExpr uexpr)) duplicatedSubtrees
              maximumAdded = 10 -- Consider at most 10 subtrees. Cuts down search.
              info = take 10 $ sortBy (\a a' -> flipOrdering $ (compare `on` snd) a a') $
                             Map.toList subtreeSizes
              bestSubtrees = trace ("INFO: " ++ show info) $ take maximumAdded $
                             map fst $
                             sortBy (\a a' -> flipOrdering $ (compare `on` snd) a a') $
                             Map.toList subtreeSizes
          in
           map ((\gr -> estimateGrammar gr pseudocounts obs) . (\expr -> Grammar appProb (Map.insert expr 0.0 lib)) . labelExpr) bestSubtrees

countSubtreesNotInGrammar :: ExprDistr -- <lib>: Distribution over expressions. 
                            -> ExprMap Double -- <cnt>: Map of rules and associated counts.  
                            -> (Expr a ,Double) -- <expr>: the expression
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
exprSize distr App{eLeft=l, eRight=r, eLabel=Nothing} = 1.0 + exprSize distr l + exprSize distr r
exprSize _ _ = 1.0 

relabelFrontier :: Grammar -> [(UExpr, a)] -> [(UExpr, a)]
relabelFrontier gr es = map go (map (first fromUExpr) es)
    where go (t@Term{}, s) = (toUExpr t, s) 
          go (a@App{}, s) = if Map.member ua (grExprDistr gr) then (labelExpr ua, s) else (unlabelExpr ua, s)
                                                                       where ua = toUExpr a
----------------------------------------------------------------------
-- Solve a single task

-- solveTask :: (MonadPlus m, MonadRandom m) => 
--         -> Int -- ^ <n> plan length
--         -> Int -- ^ <m> branching factor 
--         -> m ( [(UExpr, Double)], -- all expressions tried and associated rewards
--                [UExpr], -- plan of expressions
--                Double) -- score of plan
solveTask gr Task{task=tsk, taskType=tp} n m = go [] [] 0 n 
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



