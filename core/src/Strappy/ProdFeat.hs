                                   
-- | Production features: The features of a task is the expected counts of each production

module Strappy.ProdFeat where

import Strappy.Library
import Strappy.Expr
import Strappy.Type
import Strappy.Utils

import qualified Data.Map as Map
import Data.Maybe
import Data.List

productionVector :: Double -> Counts -> Grammar -> Expr -> Counts
productionVector wt cnts gr e@(Term {}) =
	cnts { termCounts = termCounts cnts + wt,
			   useCounts = Map.insertWith (+) e wt (useCounts cnts) }
productionVector wt cnts gr@(Grammar { grExprDistr = distr }) e@(App { eLeft = l, eRight = r})
  | not (Map.member e distr) =
    let cnts'   = cnts { appCounts = appCounts cnts + wt }
        cnts''  = productionVector wt cnts' gr l
        cnts''' = productionVector wt cnts'' gr r
    in cnts'''
productionVector wt cnts gr@(Grammar { grExprDistr = distr, grApp = logApp }) e@(App { eLeft = l, eRight = r, eReqType = Just tp }) =
  let alts = filter (\(e', _) -> canUnifyFast tp (eType e')) $ Map.toList distr
      logAltZ = logSumExpList $ map snd alts
      logPApp = fromJust (eLogLikelihood l) + fromJust (eLogLikelihood r) + logApp
      logPTerm = log (1 - exp logApp) + fromJust (Map.lookup e distr) - logAltZ
      logZ = logSumExp logPApp logPTerm
      pApp = exp (logPApp - logZ)
      pTerm = exp (logPTerm - logZ)
      -- Recurse + accumulate in cnts
      cnts'   = cnts { appCounts = appCounts cnts + wt * pApp,
                       termCounts = termCounts cnts + wt * pTerm,
                       useCounts = Map.insertWith (+) e (wt * pTerm) (useCounts cnts) }
      cnts''  = productionVector (wt * pApp) cnts' gr l
      cnts''' = productionVector (wt * pApp) cnts'' gr r
  in cnts'''

taskFeatures :: Grammar -> -- ^ Final grammar
                Type -> -- ^ Toplevel requested type
                [Expr] -> -- ^ List of programs that solve the task
                [Double] -- ^ Feature vector
taskFeatures gr tp es =
  let es' = map (exprLogLikelihood gr . annotateRequested' tp) es
      logZ = logSumExpList $ map (fromJust . eLogLikelihood) es'
      distr = map (\e -> (e, exp (fromJust (eLogLikelihood e) - logZ))) es'
      cnts = foldl (\cnt (e, wt) -> productionVector wt cnt gr e)
                   (Counts { appCounts = 0.0, termCounts = 0.0, useCounts = Map.empty,
                             possibleUseCounts = error "taskFeatures: No possible uses counted" })
                   distr
      prods = sort $ Map.keys $ grExprDistr gr
      prodFeatures = map (\prod -> Map.findWithDefault 0.0 prod (useCounts cnts)) prods
      appFeature = appCounts cnts
      termFeature = termCounts cnts
  in appFeature : termFeature : prodFeatures

-- | Human-readable explanation of the features
featureNames :: Grammar -> [String]
featureNames gr =
  "# Applications" : "# Terminals" : map show (sort $ Map.keys $ grExprDistr gr)
