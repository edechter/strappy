-- | This module compresses a set of combinators using a weighted version of Neville-Manning
-- | It finds the same solution as the corresponding linear program.

module Strappy.Compress (compressWeightedCorpus, grammarEM) where

import Strappy.Expr
import Strappy.Library
import Strappy.Utils
import Strappy.Config
import Strappy.Type

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans
import Debug.Trace
import System.IO.Unsafe
import System.CPUTime
import Data.Maybe


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


grammarEM :: Double -> -- ^ lambda
             Double -> -- ^ pseudocounts
             Grammar -> -- ^ initial grammar
             [ExprMap Double] -> -- ^ For each task, program likelihoods
             Grammar
grammarEM lambda pseudocounts g0 tsks =
  let frontiers = flip map tsks $ \lls -> Map.mapWithKey (\e ll -> ll + fromJust (eLogLikelihood (exprLogLikelihood g0 e))) lls
      zs = flip map frontiers $ Map.fold logSumExp (log 0)
      normFrontiers = zipWith (\front z -> Map.map (\l -> l - z) front) frontiers zs
      corpus = Map.toList $ Map.map exp $ foldl1 (Map.unionWith logSumExp) normFrontiers
      g' = compressWeightedCorpus lambda pseudocounts g0 corpus
      oldProductions = Set.fromList $ Map.keys $ grExprDistr g0
      newProductions = Set.fromList $ Map.keys $ grExprDistr g'
  in if oldProductions == newProductions
     then g'
     else trace "Another iter of grammarEM..." (grammarEM lambda pseudocounts g' tsks)