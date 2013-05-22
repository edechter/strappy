
module Strappy.EM where

import Strappy.Sample
import Strappy.Expr
import Strappy.Library
import Strappy.Type

import Data.Maybe
import Data.List
import Data.Function
import qualified Data.HashMap as Map
import Control.Monad
import Control.Monad.Random
import Control.Monad.State

-- Evaluates to the number of nats required to encode the grammar
-- Does not include production probabilities TODO: why not?
descriptionLength :: Grammar -> Double
descriptionLength gr@(Grammar{grExprDistr=lib}) =
  Map.foldWithKey (\ k _ s -> s + productionLen (fromUExpr k)) 0.0 lib
  where 
    -- Unprimed is invoked first, and doesn't look through grammar
    -- Primed can look through grammar for matching subtrees
    -- This is done so that we don't pick ourselves out of the grammar
    productionLen :: Expr a -> Double
    productionLen (App {eLeft=l, eRight=r}) =
      productionLen' l + productionLen' r
    productionLen _ = 0 -- Terminal production w/o application is always
                        -- in the grammar, so we don't have to count it
    productionLen' :: Expr a -> Double
    productionLen' c = exprSize lib c


-- Likelihood term in EM
-- (frontier!!i)!!k   ==   <e_i^k, P(e_i^k | t_i, G^old)>
-- The probabilities should be normalized, eg,
--    forall i.  (sum $ map snd $ frontier!!i) == 1
grammarLogLikelihood :: Grammar -> [[(UExpr,Double)]] -> Double
grammarLogLikelihood gr frontier =
  -- Calculate prob of new grammar generating each combinator
  let combLLs = map (map (\ (e,_) -> exprLogLikelihood gr (fromUExpr e))) frontier
      wMatrix = map (map snd) frontier
  in sum $ zipWith (\lls ws -> sum $ zipWith (*) lls ws) combLLs wMatrix

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
  

-- Takes a grammar and a list of tasks; updates to a new grammar
doEMIteration :: (MonadPlus m, MonadRandom m )=>
                 Double
                 -> Grammar 
                 -> [UExpr] 
                 -> Int 
                 -> [(Type, UExpr -> Double)] -> m Grammar 
doEMIteration lambda gr primitives size tasks = do
  let ms = map (sampleExprs size gr . fst) tasks  
  xs <- sequence . map sequence $ ms 
  let frontiers = map (map $ toUExpr . fst) xs :: [[UExpr]]
  -- Weight each frontier by likelihood
  let frontiers' = zipWith (\frontier (_, likelihood) ->
                             map (\ expr -> (expr, likelihood expr))
                             frontier)
                   frontiers tasks
  -- Compute normalizing constants
  let zs = map (sum . map snd) frontiers'
  -- Remove unhit tasks
  let z_and_frontier = filter ((>0) . fst) $ zip zs frontiers'
  -- Divide by normalizing constant
  let frontiers'' = map (\(z, frontier) ->
                          map (\ (expr, posterior) -> (expr, posterior/z))
                          frontier)
                    z_and_frontier
  return $ optimizeGrammar lambda gr primitives frontiers''

-- Performs hill climbing upon the given grammar
optimizeGrammar :: Double -> Grammar -> [UExpr]
                   -> [[(UExpr, Double)]] -> Grammar
optimizeGrammar lambda gr primitives frontier =
  let flatFrontier = concat frontier
      gr' = estimateGrammar gr 1.0 flatFrontier
      -- TODO: BIC or something like that for continuous params
      initial_score = lambda * (descriptionLength gr' - grammarLogLikelihood gr' frontier)
      -- This procedure performs hill climbing
      climb :: Grammar -> Double -> Grammar
      climb g g_score =
        let gs = grammarNeighbors g primitives frontier
            gsScore = [ (g, lambda * descriptionLength g - grammarLogLikelihood g frontier) |
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
grammarNeighbors :: Grammar -> [UExpr] -> [[(UExpr, Double)]] ->
                    [Grammar]
grammarNeighbors (Grammar appProb lib) primitives obs = oneRemoved ++ oneAdded
  where oneRemoved = map (\expr -> Grammar appProb (Map.delete expr lib)) $
                         (Map.keys lib) \\ primitives
        oneAdded =
          let duplicatedSubtrees =
                foldl (\cnt expr_wt -> countSubtreesNotInGrammar lib cnt ((\(e, w) -> (fromUExpr e, w) )$ expr_wt)) Map.empty (join obs) 
              subtreeSizes = Map.mapWithKey (\uexpr cnt -> cnt * (exprSize lib (fromUExpr uexpr)))
                                           $ duplicatedSubtrees
              maximumAdded = 10 -- Consider at most 10 subtrees. Cuts down search.
              bestSubtrees = take maximumAdded $
                             map fst $
                             sortBy (compare `on` snd) $
                             Map.toList subtreeSizes
          in
           map (\expr -> Grammar appProb (Map.insert expr 0.0 lib)) bestSubtrees

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