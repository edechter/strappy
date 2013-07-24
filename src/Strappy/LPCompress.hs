-- | This module compresses a set of combinators by solving the corresponding linear program.

module Strappy.LPCompress (compressWeightedCorpus) where

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
      (grammar''', corpus') = if compressLibrary
                              then let (compProds, subseq) = simplifyLibrary productions
                                       compCorpus = map (\(e,w) -> (foldl (\ce (old,new) -> subExpr old new ce) e subseq, w)) corpus
                                       -- VERY VERY VERY BAD
                                       -- THE TYPE SHOULD NOT BE HARD CODED
                                       -- FIXME TODO FIXME
                                       compCorpus' = map (\(e, w) -> (annotateRequested' (tInt ->- tInt) e, w)) compCorpus
                                       uniLogProb = - log (genericLength compProds)
                                   in (Grammar (log 0.5) $ Map.fromList [ (prod, uniLogProb) | prod <- compProds ], compCorpus')
                              else (grammar'', corpus)
      grammar'''' = inoutEstimateGrammar grammar''' pseudocounts corpus'
  in grammar''''

-- Weighted Nevill-Manning
compressCorpus :: Double -> ExprMap Double -> [Expr]
compressCorpus lambda counts =
  map fst $ filter (\(_, c) -> c >= lambda) $ Map.toList counts
