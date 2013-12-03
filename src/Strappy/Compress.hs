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
import qualified Data.Array.IArray as Array


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
             [(ExprMap Double, Int)] -> -- ^ For each task, program likelihoods and multiplicative counts
             Grammar
grammarEM lambda pseudocounts g0 tsks =
  let frontiers = flip map tsks $ \(lls, _) -> Map.mapWithKey (\e ll -> ll + fromJust (eLogLikelihood (exprLogLikelihood g0 e))) lls
      zs = flip map frontiers $ Map.fold logSumExp (log 0)
      normFrontiers = zipWith3 (\front z (_, cnt) -> Map.map (\l -> fromIntegral cnt * exp (l - z)) front) frontiers zs tsks
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
                    [(ExprMap Double, Int)] -> -- ^ For each task, program likelihoods and multiplicative counts
                    Grammar
grammarHillClimb lambda pseudocounts g0 tsks =
  let tsks' = map (Map.keys . fst) tsks
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
      -- Hill climbing
      flags2posterior = compileLogPosterior tsks' seedPrims candidateFrags'
      deps = makeDependencyArray candidateFrags'
      succs = grammarSuccessors deps
      flags = Array.listArray (0, length candidateFrags' - 1) $ replicate (length candidateFrags') False :: Array.Array Int Bool
      bestVector = climb flags2posterior succs flags (flags2posterior flags)
      bestFrags = vec2lib bestVector candidateFrags'
      -- Estimate grammar params
      g = Grammar { grExprDistr = Map.fromList [ (l, 0.0) | l <- (bestFrags ++ seedPrims) ],
                    grApp = log 0.5 }
      ts' = [ [ (e, fromJust $ eLogLikelihood $ exprLogLikelihood g e) | e <- front ] | front <- tsks' ]
      logZs = map (logSumExpList . map snd) ts'
      ts'' = zipWith (\front logZ -> map (\(e, w) -> (e, exp (w-logZ))) front) ts' logZs
      corpus = concat ts''
  in trace ("Num fragaments:" ++ show (length candidateFrags)) $
           inoutEstimateGrammar g pseudocounts corpus
  where climb :: (Array.Array Int Bool -> Double) -> -- ^ Scoring function
                 (Array.Array Int Bool -> [Array.Array Int Bool]) -> -- ^ Successorship function
                 Array.Array Int Bool -> -- ^ current state
                 Double  -> -- ^ Log posterior under previous grammar
                 Array.Array Int Bool -- ^ Final result
        climb lp succs curr oldLP =
          let newStates = succs curr
              newLLs = map lp newStates
              (newLib, newLP) = maximumBy (compare `on` snd) (zip newStates newLLs)
          in if newLP > oldLP
             then climb lp succs newLib newLP
             else curr
        compileLogPosterior :: [[Expr]] -> [Expr] -> [Expr] -> Array.Array Int Bool -> Double
        compileLogPosterior solns prims fs =
          let cts = map (compileTaskLL prims fs) solns
          in \flags -> -lambda * vecHamming flags + sum (map ($flags) cts)
        makeDependencyArray :: [Expr] -> Array.Array Int [Int]
        makeDependencyArray fs =
          let getD idx = case (fs !! idx) of
                           Term {} -> []
                           App { eLeft = Term {}, eRight = Term {} } -> []
                           App { eLeft = Term {}, eRight = r@(App {}) } ->
                             [listIdx r fs]
                           App { eLeft = l@(App {}), eRight = Term {} } ->
                             [listIdx l fs]
                           App { eLeft = l@(App {}), eRight = r@(App {}) } ->
                             [listIdx l fs, listIdx r fs]
          in Array.array (0, length fs - 1)
                      [ (idx, getD idx) | idx <- [0..length fs - 1] ]
        getDependencies :: Array.Array Int [Int] -> Array.Array Int Bool ->
                           Array.Array Int Bool
        getDependencies deps flags =
          let collectFlags idx =
                concatMap (\i -> if flags Array.! i then [] else i : collectFlags i) (deps Array.! idx)
          in flags Array.// [ (idx, True) | i <- [0..snd (Array.bounds deps)], flags Array.! i, idx <- collectFlags i]
        vec2lib :: Array.Array Int Bool -> [Expr] -> [Expr]
        vec2lib v fs = map fst $ filter (\(_, idx) -> v Array.! idx) $ zip fs [0..]
        -- hamming weight
        vecHamming :: Array.Array Int Bool -> Double
        vecHamming flags = foldl (\acc i -> if flags Array.! i then acc+1.0 else acc) 0.0 [0..snd (Array.bounds flags)]
        grammarSuccessors :: Array.Array Int [Int] -> Array.Array Int Bool -> [Array.Array Int Bool]
        grammarSuccessors deps flags =
          map (\toflag -> getDependencies deps $ flags Array.// [(toflag, True)]) $
            filter (not . (flags Array.!)) [0..snd (Array.bounds flags)]


log2 = log 2.0

compileTaskLL :: [Expr] -> -- ^ Primitives
                 [Expr] -> -- ^ Candidate library procedures
                 [Expr] -> -- ^ expressions solving task
                 Array.Array Int Bool -> -- ^ vector of library flags
                 Double -- ^ log likelihood of expression
compileTaskLL prims candidates solns =
  let cs = map (compileLL prims candidates) solns
  in \flags -> logSumExpList $ map ($flags) cs

compileLL :: [Expr] -> -- ^ Primitives
             [Expr] -> -- ^ Candidate library procedures
             Expr -> -- ^ expression
             Array.Array Int Bool -> -- ^ vector of library flags
             Double -- ^ log likelihood of expression
compileLL prims candidates (Term { eReqType = Just tp }) =
  let conflictingPrims = genericLength $ filter (canUnifyFast tp . eType) prims
      conflictingIndexes = filter (\idx -> canUnifyFast tp $ eType $ candidates !! idx) [0 .. length (candidates) - 1] :: [Int]
  in \flags ->
        let conflicts = conflictingPrims + genericLength (filter (flags Array.!) conflictingIndexes)
        in - log2 - log conflicts
compileLL prims candidates e@(App { eLeft = l, eRight = r, eReqType = Just tp }) | e `elem` candidates =
  let conflictingPrims = genericLength $ filter (canUnifyFast tp . eType) prims
      conflictingIndexes = filter (\idx -> canUnifyFast tp $ eType $ candidates !! idx) [0 .. length (candidates) - 1]
      lComp = compileLL prims candidates l
      rComp = compileLL prims candidates r
      myIdx = listIdx e candidates
  in \flags ->
        let lLL = lComp flags
            rLL = rComp flags
            appLL = -log2 + lLL + rLL
        in if flags Array.! myIdx
           then let conflicts = conflictingPrims + genericLength (filter (flags Array.!) conflictingIndexes)
                in logSumExp (- log2 - log conflicts) appLL
           else appLL
compileLL prims candidates (App { eLeft = l, eRight = r, eReqType = tp })  =
  let lComp = compileLL prims candidates l
      rComp = compileLL prims candidates r
  in \flags ->
        let lLL = lComp flags
            rLL = rComp flags
        in -log2 + lLL + rLL