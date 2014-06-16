-- GrammarEM.hs
-- |
-- Module:      Strappy.Core.GrammarEM
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | This module implements the EM algorithm for estimating the
-- production weights of a grammar given a corpus of programs. We use
-- the inside-outside algorithm to estimate the expected counts of
-- each rule on each iteration. See
-- <http://www.cs.columbia.edu/~mcollins/io.pdf> for some notes on EM
-- and inside-outside for grammar estimation.


module Strappy.GrammarEM where

import Data.Maybe 
import qualified Data.Map as Map hiding ((\\))
import Data.Set (Set())
import qualified Data.Set as Set
import Data.Hashable
import GHC.Prim
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.List as List
import Data.List ((\\))
import Text.Printf
import Data.Function (on)
import Control.Monad.Identity
import Control.Monad.State
import Control.Arrow (first)
import Debug.Trace
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Directory
import Data.String (IsString)
import Control.Monad.Error.Class

-- Strappy imports -- 
import Strappy.Type
import Strappy.Expr
import Strappy.Grammar

-- TODO: EM should be terminated by a convergence criteria and we
-- should have at least a few random restarts.

-- | Run EM for some number of iterations. Iteratively perform the
-- inside-out algorithm on a corpus. 
em :: Int     -- ^ number of iterations of EM
   -> Grammar -- ^ initial grammar
   -> Double           -- ^ pseudocounts corresponding to flat prior on rule probabilities
   -> [(Expr, Double)] -- ^ corpus: a list of expressions and associated loglikelihood scores
   -> Grammar 
em k g prior obs =
  foldl' (\g' _ -> inoutEstimateGrammar g' prior obs) g [1..k]

data Counts = Counts { appCounts :: !Double,
                       termCounts :: !Double,
                       useCounts :: !ExprMap Double,
                       possibleUseCounts :: !ExprMap Double } deriving Show

initializeCounts :: Double -- ^ pseudocounts
                 -> Counts
initializeCounts alpha = Counts alpha alpha Map.empty Map.empty

-- | A data type for an expression in a corpus of elements. 
data CorpusElem = CorpusElem Expr -- ^ an expression in a corpus
                             Type -- ^ the type of the expression when it was requested
                             Double -- ^ the score of the expression on a task (interpreted as a loglikelihood)
                             deriving Show

type Corpus = [CorpusElem]

-- | Return the logPriors associated with a corpus
exprLogPriors :: MonadError String m => Grammar -> Corpus -> m [Double]
exprLogPriors gr corpus = sequence $ [exprLogLikelihood gr tp e | (CorpusElem e tp _) <- es]

-- | Calculate the expected counts of each production in a grammar given a corpus. 
expectedCounts :: MonadError String m
               => Grammar
               -> Double -- ^ pseudocounts
               -> Corpus
               -> m Counts
expectedCounts gr alpha corpus = go (initializeCounts alpha) corpus
  where go !counts [] = counts
        go !counts ((Corpus expr tp w): cs)  = go (updateExpectedCounts counts expr tp w) cs 

-- | Update the counts datastructure with an additional weighted expression.
updateExpectedCounts :: MonadError String m
                        => Grammar -> Counts -> Expr -> Type -> Weight -> m Counts
updateExpectedCounts gr counts expr w = evalTI $ go counts expr w
  where go :: MonadError String m
              => Counts -> Expr -> Type -> Weight -> TypeInference m Counts
        go counts e@Term{} tp w =  do
          ll <- logLikelihoodPrimM
          let logpNoApp = log (1 - exp gamma)
          x = exp ( logpNoApp + ll)
          Map.insertWith 

          

-- | Run the inside-out algorithm to estimate production probabilities.
insideOutside :: MonadError String m =>
                 Grammar -- ^ input grammar
                 -> Double  -- ^ alpha: pseudocounts corresponding to flat prior on rule probabilities
                 -> Corpus 
                 -> m Grammar          
insideOutside gr@(Grammar app distr) alpha corpus = do
  counts <- expectedCounts gr corpus
  log
  where es = Map.toList distr -- [(Expr, Double)]
        counts = expectedCounts


        counts = List.foldl' (expectedCounts cts ) (initializeCounts alpha) (zip obs logPriors) 
        allExprs = Map.keys distr ++ Map.
        possibleUses' = List.foldl' (\cts e -> Map.insertWith (+) e pseudocounts cts)
                                    (possibleUseCounts counts) $ Map.keys distr
        logTotalUses = log $ sum $ Map.elems uses'
        appLogProb = log (appCounts counts) - log (termCounts counts + appCounts counts)
        distr' = Map.mapWithKey (\expr _ ->
                                  if usePCFGWeighting
                                  then case Map.lookup expr uses' of
                                    Just u -> log u - logTotalUses
                                    Nothing -> error "Should never occur: expr not in uses or possible uses"
                                  else case (Map.lookup expr uses', Map.lookup expr possibleUses') of 
                                    (Just u, Just p) -> log u - log p
                                    _ -> error "Should never occur: expr not in uses or possible uses") distr

        -- -- Updates expected counts
        -- expectedCounts :: Double -> Counts -> Expr -> Counts
        -- expectedCounts weight counts expr@(Term { eReqType = Just tp }) =
        --   let uc' = Map.insertWith (+) expr weight $ useCounts counts
        --       alts = filter (\(e', _) -> canUnifyFast tp (eType e')) es
        --       pc = possibleUseCounts counts
        --       pc' = foldl (\acc alt -> Map.insertWith (+) alt weight acc) pc $ map fst alts
        --       logZ = if usePCFGWeighting then 0.0 else logSumExpList (map snd alts)
        --       counts' = counts { termCounts = termCounts counts + weight,
        --                          useCounts = uc', 
        --                          possibleUseCounts = pc' }
        --   in counts'
        -- expectedCounts weight counts expr@(App { eLeft = left,
        --                                          eRight = right, 
        --                                          eReqType = Just tp }) | Map.member expr distr =
        --   let alts = filter (\(e', _) -> canUnifyFast tp (eType e')) es
        --       logZ = if usePCFGWeighting then 0.0 else logSumExpList (map snd alts)
        --       leftLL  = fromJust $ eLogLikelihood left
        --       rightLL = fromJust $ eLogLikelihood right
        --       -- Find probability of having used an application vs a library procedure
        --       logProbLib = distr Map.! expr + log (1 - exp app) - logZ
        --       logProbApp = app + leftLL + rightLL
        --       probUsedApp = exp $ logProbApp - logSumExp logProbApp logProbLib
        --       probUsedLib = 1 - probUsedApp
        --       -- Recurse on children
        --       counts'  = expectedCounts (weight*probUsedApp) counts left
        --       counts'' = expectedCounts (weight*probUsedApp) counts right
        --       -- Add in counts for if we used a library procedure
        --       uc' = Map.insertWith (+) expr (weight*probUsedLib) $ useCounts counts''
        --       pc  = possibleUseCounts counts''
        --       pc' = foldl (\acc alt -> Map.insertWith (+) alt (weight*probUsedLib) acc) pc $ map fst alts
        --       counts''' = counts'' { appCounts = appCounts counts'' + weight * probUsedApp,
        --                              termCounts = termCounts counts'' + weight * probUsedLib, 
        --                              useCounts = uc', 
        --                              possibleUseCounts = pc' }
        --   in counts'''
        -- expectedCounts weight counts expr@(App { eLeft = left,
        --                                          eRight = right, 
        --                                          eReqType = Just _}) =
        --    let counts'   = counts { appCounts = appCounts counts + weight }
        --        counts''  = expectedCounts weight counts' left
        --        counts''' = expectedCounts weight counts'' right
        --   in counts'''
