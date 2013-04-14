
module Strappy.EM where

import Strappy.Grammar
import Strappy.CL
import Strappy.EnumBF
import Strappy.Type
import qualified Strappy.CombMap as CM

import Data.Maybe
import Data.List
import Data.Function
import qualified Data.Map as Map

-- Evaluates to the number of nats required to encode the grammar
-- Does not include production probabilities
descriptionLength :: Grammar -> Double
descriptionLength gr@(Grammar{library=lib}) =
  CM.foldWithKey (\ k _ s -> s + productionLen k) 0.0 lib
  where 
    -- Unprimed is invoked first, and doesn't look through grammar
    -- Primed can look through grammar for matching subtrees
    -- This is done so that we don't pick ourselves out of the grammar
    productionLen :: Comb -> Double
    productionLen (CApp {lComb=l, rComb=r}) =
      productionLen' l + productionLen' r
    productionLen (CHole {}) = error "Hole inside of grammar"
    productionLen _ = 0 -- Terminal production w/o application is always
                        -- in the grammar, so we don't have to count it
    productionLen' :: Comb -> Double
    productionLen' c = - combinatorLL gr c


-- Likelihood term in EM
-- (frontier!!i)!!k   ==   <e_i^k, P(e_i^k | t_i, G^old)>
-- The probabilities should be normalized, eg,
--    forall i.  (sum $ map snd $ frontier!!i) == 1
grammarLogLikelihood :: Grammar -> [[(Comb,Double)]] -> Double
grammarLogLikelihood gr frontier =
  -- Calculate prob of new grammar generating each combinator
  let combLLs = map (map (\ (c,_) -> combinatorLL gr c)) frontier
      wMatrix = map (map snd) frontier
  in sum $ zipWith (\lls ws -> sum $ zipWith (*) lls ws) combLLs wMatrix

-- "Samples" the frontier of the old grammar
-- Does so by doing a BF enumeration and then weighting
-- Returns a list of tuples of (combinator, prior probability)
-- In the future, it would be nice to actually do a random sample from G
sampleFrontier :: Grammar -> Int -> Type -> [(Comb,Double)]
sampleFrontier gr size ty =
  let sample = [ (comb cb, value cb) | cb <- enumBF gr size ty ]
      logZ = foldl1 logSumExp $ map snd sample
      -- Normalized sample
      sample' = [ (c, exp (p - logZ)) | (c, p) <- sample ]
  in sample'
  where logSumExp :: Double -> Double -> Double
        logSumExp x y | x <  y = x + (log $ 1.0 + exp (y - x))
                      | y <= x = y + (log $ 1.0 + exp (x - y))

-- Takes a grammar and a list of tasks; updates to a new grammar
doEMIteration :: Grammar -> CM.CombMap [Type] -> Int ->
                 [(Type, Comb -> Double)] -> Grammar
doEMIteration gr baseGrammar size tasks =
  let frontiers = map (sampleFrontier gr size . fst) tasks
      -- Weight each frontier by likelihood
      frontiers' = zipWith (\frontier (_, likelihood) ->
                             map (\ (comb, prior) ->
                                   (comb, prior * likelihood comb))
                             frontier)
                   frontiers tasks
      -- Compute normalizing constants
      zs = map (sum . map snd) frontiers'
      -- Remove unhit tasks
      z_and_frontier = filter ((>0) . fst) $ zip zs frontiers'
      -- Divide by normalizing constant
      frontiers'' = map (\(z, frontier) ->
                          map (\ (comb, posterior) -> (comb, posterior/z))
                          frontier)
                    z_and_frontier
  in optimizeGrammar gr baseGrammar frontiers''

-- Performs hill climbing upon the given grammar
optimizeGrammar :: Grammar -> CM.CombMap [Type]
                   -> [[(Comb, Double)]] -> Grammar
optimizeGrammar gr baseGrammar frontier =
  let flatFrontier = concat frontier
      gr' = estimateGrammarWeighted gr 1.0 baseGrammar flatFrontier
      -- TODO: BIC or something like that for continuous params
      initial_score = descriptionLength gr' - grammarLogLikelihood gr' frontier
      -- This procedure performs hill climbing
      climb :: Grammar -> Double -> Grammar
      climb g g_score =
        let gs = grammarNeighbors g frontier
            gsScore = [ (g, descriptionLength g - grammarLogLikelihood g frontier) |
                        g <- gs ]
            (best_g', best_g'_score) = minimumBy (compare `on` snd) gsScore
        in
         if best_g'_score < g_score
         then climb best_g' best_g'_score
         else g
  in
   climb gr' initial_score

grammarNeighbors :: Grammar -> [[(Comb, Double)]] -> [Grammar]
grammarNeighbors = undefined