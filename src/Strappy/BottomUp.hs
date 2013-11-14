
module Strappy.BottomUp where
--module Main where

import Strappy.Expr
import Strappy.Type
import Strappy.Library
import Strappy.Simplify
import Strappy.ExprTemplate

import Data.List
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQ
import Data.Maybe


getTemplates :: Expr -> [(Expr, [Expr])]
getTemplates e =
    let arity = getArity e
    in filter (not . null . snd) $ flip map [0..arity] $ \numArgs ->
              let args = map eVar $ map (\x -> -x) [1..numArgs]
                  e' = foldl (<>) e args
                  templates = collectTemplates e'
              in (e', templates)
    where collectTemplates e' =
            case reduceExpr e' of
              Nothing -> []
              Just e'' -> e'' : collectTemplates e''

-- | Append is special, in that it has a conditional branch.
-- getTemplates won't work on it.
appendTemplates :: [(Expr, [Expr])]
appendTemplates = [(cAppend <> cEmpty <> (eVar $ -1),
                    [eVar $ -1]),
                   ((cAppend <> ((cCons <> (eVar $ -1)) <> (eVar $ -2))) <> (eVar $ -3),
                    [((cCons <> (eVar $ -1)) <> ((cAppend <> (eVar $ -2)) <> (eVar $ -3)))])]


invertRewrites :: Expr -> -- ^ Original expression
                  [(Expr, [Expr])] -> -- ^ Rewrites
                  [Expr] -- ^ Equivalent expressions
invertRewrites expr = map canonicalizeEVars . concatMap (invert expr)
  where invert :: Expr -> (Expr, [Expr]) -> [Expr]
        invert e@(Term {}) rs = invert1 e rs
        invert e@(App { eLeft = l, eRight = r}) rs =
          invert1 e rs ++
          map (\l' -> e { eLeft = l' }) (invert l rs) ++
          map (\r' -> e { eRight = r' }) (invert r rs)
        invert1 :: Expr -> (Expr, [Expr]) -> [Expr]
        invert1 _ (_, []) = []
        invert1 target (lhs, rhs:rhss) =
          let action = do unifyE target rhs
                          lhs' <- applyESub lhs
                          return $ canonicalizeEVars lhs'
          in case runET action of
              Nothing -> invert1 target (lhs, rhss)
              Just lhs' -> [lhs']

-- Approximate calculation of the MDL of any expression matching the template
-- TODO: Make use of typing info to get a better estimate of MDL
-- The only reason this calculation is approximate is because exact computation is annoying to implement.
templateMDL :: Grammar -> Type -> Expr -> Maybe Double
templateMDL (Grammar { grApp = logApp, grExprDistr = distr }) tp expr = runTI (mdl tp expr)
  where mdl _ (Term { eName = '?':_ }) = return minMDL
        mdl t e =
          case filter (canMatchTemplate e . fst) distrList of
            matches@(_:_) -> do
              return $ termMDL - maximum (map snd matches)
            [] ->
              case e of
                Term {} -> error "templateMDL: Term not in library"
                App { eLeft = l, eRight = r } -> do
                  lMDL <- mdl undefined l
                  rMDL <- mdl undefined r
                  return $ lMDL + rMDL + appMDL
        -- Approximate in the case of repeated variables in the template
        canMatchTemplate :: Expr -> -- ^ Template
                            Expr -> -- ^ Candidate expression
                            Bool
        canMatchTemplate (Term { eName = '?':_ }) _ = True
        canMatchTemplate e1@(Term { }) e2 = e1 == e2
        canMatchTemplate (App { eLeft = l, eRight = r }) (App { eLeft = l', eRight = r' }) =
          canMatchTemplate l l' && canMatchTemplate r r'
        canMatchTemplate _ _ = False
        appMDL :: Double
        appMDL = -logApp
        termMDL :: Double
        termMDL = -log(1-exp logApp)
        distrList = M.toList distr
        minMDL :: Double
        minMDL = minimum $ map snd distrList

enumBU :: Int -> Grammar -> Type -> Expr -> IO [Expr]
enumBU sz gr tp seed =
  let open = PQ.singleton (fromJust $ templateMDL gr tp seed,
                           seed)
      closed = S.singleton seed
  in liftM (filter typeChecks) $ liftM concat $ mapM instantiateVars $ S.toList $ bu open closed
  where bu open closed | PQ.size open == 0 || S.size closed >= sz = closed
        bu open closed =
          let ((_, cb), open') = PQ.deleteFindMin open -- get best open solution
              children = invertRewrites cb templates
              children' = map (\child -> (templateMDL gr tp seed, child)) children
              children'' = map (\(maybeMDL, child) -> (fromJust maybeMDL, child)) $
                           filter (isJust . fst) children'
              children''' = filter (\child -> not (S.member (snd child) closed)) children''
              closed' = foldl (\acc child -> S.insert (snd child) acc) closed children'''
              open'' = foldl (\acc kid -> PQ.insert kid acc) open' children'''
          in bu open'' closed'
        templates = appendTemplates ++ concatMap getTemplates (map fst $ M.toList $ grExprDistr gr)
        instantiateVars e =
          case getEVars e of
            [] -> return [e]
            _ -> error "Variables not currently handled"
{-
main = do
    let rs = appendTemplates ++ concatMap getTemplates [cS, cB, cI, cC]
    let expr = readExpr "((: 0) ((: 0) []))"
    forM_ (invertRewrites expr rs) $ putStrLn . show
-}