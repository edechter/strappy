-- | This module compresses a set of combinators using a weighted version of Neville-Manning
-- | It finds the same solution as the corresponding linear program.

module Strappy.Compress (compressWeightedCorpus, grammarEM, grammarHillClimb) where

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
import Data.Function


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
      normFrontiers = zipWith (\front z -> Map.map (\l -> exp (l - z)) front) frontiers zs
      corpus = Map.toList $ foldl1 (Map.unionWith (+)) normFrontiers
      g' = compressWeightedCorpus lambda pseudocounts g0 corpus
      oldProductions = Set.fromList $ Map.keys $ grExprDistr g0
      newProductions = Set.fromList $ Map.keys $ grExprDistr g'
  in if oldProductions == newProductions
     then g'
     else trace ("Another iter of grammarEM...\n" ++ showGrammar g') $ grammarEM lambda pseudocounts g' tsks



-- | In this procedure, likelihoods are ignored.
grammarHillClimb :: Double -> -- ^ lambda
                    Double -> -- ^ pseudocounts
                    Grammar -> -- ^ initial grammar
                    [ExprMap Double] -> -- ^ For each task, program likelihoods
                    Grammar
grammarHillClimb lambda pseudocounts g0 tsks =
  let tsks' = map Map.keys tsks
      -- chop each task up in to its constituent program fragments
      frags = map (Set.toList . foldl collectSubtrees Set.empty) tsks'
      -- find only those fragments that occur in more than one task
      candidateFrags = Map.keys $ Map.filter (>1) $
                       foldl (\acc fs ->
                               foldl (\a f -> Map.insertWith (+) f 1 a)
                                     acc fs)
                             Map.empty frags
      candidateFrags' = map (\e -> e { eType = doTypeInference e }) candidateFrags
      seedPrims = Map.keys $ grExprDistr g0
      lp = logPosterior tsks' seedPrims
  in trace ("Num fragaments:" ++ show (length candidateFrags)) $
           climb tsks' seedPrims lp candidateFrags'
  where climb :: [[Expr]] -> -- ^ Programs that solve each task
                 [Expr] -> -- ^ Current library
                 Double  -> -- ^ Log posterior under previous grammar
                 [Expr] -> -- ^ Program fragments we might consider adding
                 Grammar -- ^ Final grammar
        climb ts ps oldLP frags =
          -- For each fragment, consider what the library would look like with it added
          let newPs = map (add2lib ps) frags
              newLLs = map (logPosterior ts) newPs
              (newLib, newLP) = maximumBy (compare `on` snd) (zip newPs newLLs)
              newFrags = filter (not . flip elem newLib) frags
          in if newLP > oldLP
             then trace ("New library:" ++ show newLib ++ "\t+" ++ show (newLP - oldLP)) $ climb ts newLib newLP newFrags
             else let g = Grammar { grExprDistr = Map.fromList [ (l, 0.0) | l <- ps],
                                    grApp = log 0.5 }
                      ts' = [ [ (e, fromJust $ eLogLikelihood $ exprLogLikelihood g e) | e <- front ] | front <- ts ]
                      logZs = map (logSumExpList . map snd) ts'
                      ts'' = zipWith (\front logZ -> map (\(e, w) -> (e, exp (w-logZ))) front) ts' logZs
                      corpus = concat ts''
                  in inoutEstimateGrammar g pseudocounts corpus
        add2lib :: [Expr] -> Expr -> [Expr]
        add2lib ps e =
          let newPs (Term {}) = []
              newPs e'@(App { eLeft = l, eRight = r}) =
                if e' `elem` ps
                then []
                else e' : nub (newPs l ++ newPs r)
              additions = newPs e
          in additions ++ ps
        logPosterior :: [[Expr]] -> [Expr] -> Double
        logPosterior solns lib = -lambda * genericLength lib + sum (map (logSumExpList . map (negate . approxMDL lib)) solns)




-- | With uniform production probabilities, what is the MDL of the expression?
approxMDL :: [Expr] -> Expr -> Double
approxMDL lib (Term { eReqType = Just tp }) =
  log2 + log (genericLength $ filter (canUnifyFast tp . eType) lib)
approxMDL lib e@(App { eReqType = Just tp, eLeft = l, eRight = r }) =
  log2 + if e `elem` lib
         then log $ genericLength $ filter (canUnifyFast tp . eType) lib
         else approxMDL lib l + approxMDL lib r