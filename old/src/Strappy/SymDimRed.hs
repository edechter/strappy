-- | Symbolic dimensionality reduction

module Strappy.SymDimRed where

import Strappy.Expr
import Strappy.Type
import Strappy.Library
import Strappy.Simplify
import Strappy.ExprTemplate
import Strappy.Utils

import Data.Maybe
import Data.Function
import Data.List
import qualified Data.Map as Map

mlDecoder :: Grammar -> Type -> [[Expr]] -> Expr
mlDecoder gr tp ess =
  let decoders = map (concatMap getDecoders) ess
      -- only consider those decoders that are used in every task
      candidateDecoders = nub $ concatMap (map fst) decoders
      finalDecoders = filter (\decoder -> all (elem decoder . map fst) decoders) candidateDecoders
  in if null finalDecoders
     then cI -- Failure
     else fst $
          maximumBy (compare `on` snd) $
          map (\d -> (d, decoderPosterior gr tp decoders d)) finalDecoders

getDecoders :: Expr -> [(Expr, [Expr])]
getDecoders expr = filter (not . null . snd) $ decs [] expr
  where decs as e@(Term {}) = [(e, as)]
        decs as e@(App { eLeft = l, eRight = r}) =
          (e, as) : decs (r:as) l

decoderPosterior :: Grammar -> Type -> [[(Expr, [Expr])]] -> Expr ->  Double
decoderPosterior g tp tsks dec =
  let ll = sum $ map (decoderTaskLL g tp dec) tsks
      prior = fromJust $ eLogLikelihood $ exprLogLikelihood g $ annotateRequested' (t1 ->- t2) dec
  in ll + prior

decoderTaskLL :: Grammar -> Type -> Expr -> [(Expr, [Expr])] -> Double
decoderTaskLL g tp dec solns =
  logSumExpList $ map (getArgLikelihood g tp dec . snd) $ filter ((==dec) . fst) solns

getArgLikelihood :: Grammar -> Type -> Expr -> [Expr] -> Double
getArgLikelihood (Grammar { grExprDistr = distr, grApp = logApp }) tp decoder args =
  safeFromJust "getArgLikelihood: got Nothing" $ runTI $ do
    tp' <- doTypeInferenceM decoder
    lik decoder tp' args
  where lik :: Expr -> Type -> [Expr] -> TypeInference Maybe Double
        lik _ _ [] = return 0.0
        lik dec dTp [a] = do
          alpha <- mkTVar
          unify dTp (alpha ->- tp)
          alpha' <- applySub alpha
          eLL alpha' a
        lik dec dTp (a:as) = do
          alpha <- mkTVar
          beta <- mkTVar
          unify dTp (alpha ->- beta)
          alpha' <- applySub alpha
          beta' <- applySub beta
          aLL <- eLL alpha' a
          asLL <- lik (dec <> a) beta' as
          return $ aLL + asLL
        eLL :: Type -> Expr -> TypeInference Maybe Double
        eLL rTp e@(Term { eType = tTp }) = do
          rTp' <- applySub rTp
          let alts = filter (\(e', _) -> canUnifyFast rTp' (eType e')) $ Map.toList distr
          let zT = logSumExpList $ map snd alts
          let logTerm = log (1 - exp logApp)
          tTp' <- instantiateType tTp
          unify rTp' tTp'
          return $ (distr Map.! e) + logTerm - zT
        eLL reqTp e@(App { eLeft = l, eRight = r }) = do
          reqTp' <- applySub reqTp
          alpha <- mkTVar
          lLL <- eLL (alpha ->- reqTp') l
          alpha' <- applySub alpha
          rLL <- eLL alpha' r
          case Map.lookup e distr of
            Nothing -> return $ logApp + lLL + rLL
            Just myLL -> let alts = filter (\(e, _) -> canUnifyFast reqTp' (eType e)) $ Map.toList distr
                             zA = logSumExpList $ map snd alts
                             logTerm = log (1 - exp logApp)
                         in return $ logSumExp (myLL + logTerm - zA) (logApp + lLL + rLL)