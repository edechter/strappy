
module Strappy.BottomUp where
--module Main where

import Strappy.Expr
import Strappy.Type
import Strappy.Library
import Strappy.Simplify
import Strappy.ExprTemplate
import Strappy.Utils
import Strappy.Config
import Strappy.Compress

import Data.List
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.PQueue.Max (MaxQueue)
import qualified Data.PQueue.Max as PQ
import Data.Maybe
import Data.Function


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

enumBU :: Int -> -- ^ Number programs enumerated
          Int -> -- ^ Of those programs, how many do we keep?
          Grammar -> -- ^ Starting grammar
          Type -> -- ^ Requested type
          Expr -> -- ^ Initializing expression; evaluates to what we want to enumerate, and is in normal form
          IO (ExprMap Double)
enumBU sz szKept gr tp seed =
  let open = PQ.singleton (getLL seed, seed)
      closed = S.singleton seed
  in do es <- liftM concat . mapM instantiateVars . S.toList =<< bu open closed
        let distr = [ (e', fromJust (eLogLikelihood e')) | e <- es, let e' = exprLogLikelihood gr (annotateRequested' tp e) ]
        let distr' = take szKept $ sortBy (\ (_, ll) (_, ll') -> compare ll' ll) distr
        -- Normalize
        let logZ = logSumExpList $ map snd distr'
        let distr'' = [ (e, ll-logZ) | (e, ll) <- distr' ]
        return $ M.fromList distr''
  where bu open closed | PQ.size open == 0 || S.size closed >= sz = return closed
        bu open closed =
          let ((_, cb), open') = PQ.deleteFindMax open -- get best open solution
              children = invertRewrites cb templates
              children' = filter (not . flip S.member closed) children
              children'' = filter typeChecks children'
              children''' = map (\child -> (getLL child, child)) children''
              closed' = foldl (\acc child -> S.insert (snd child) acc) closed children'''
              open'' = foldl (\acc kid -> PQ.insert kid acc) open' children'''
          in do forceShowHack children'''
                bu open'' closed'
        templates = appendTemplates ++ concatMap getTemplates (map fst $ M.toList $ grExprDistr gr)
        instantiateVars e =
          case getEVars e of
            [] -> return [e]
            _ -> error "enumBU: Variables not currently handled"
        getLL :: Expr -> Double
        getLL e = fromJust $ eLogLikelihood $ exprLogLikelihood gr $ annotateRequested' tp e
{-
main = do
    let rs = appendTemplates -- ++ concatMap getTemplates [cS, cB, cI, cC]
    let expr = readExpr "((: 'a') ((: 'n') ((: 't') ((: 'i') ((: 'b') [])))))"
    let seed = Grammar { grApp = log 0.35,
                         grExprDistr = M.fromList [ (annotateRequested e, 1.0) | e <- wordExprs ] }
    es <- enumBU 15000 500 seed (tList tChar)  expr
    let es' = sortBy (compare `on` snd) $ M.toList es
    forM_ es' $ putStrLn . show . fst
-}


-- | Performs one iteration of Bottom-Up EM
doBUIter :: String -- ^ Prefix for log output
            -> [(Type, Expr, String)] -- ^ Tasks
            -> Double -- ^ Lambda
            -> Double -- ^ pseudocounts
            -> Int -- ^ frontier size
            -> Int -- ^ size kept
            -> Grammar -- ^ Initial grammar
            -> IO Grammar -- ^ Improved grammar
doBUIter prefix tasks lambda pseudocounts frontierSize keepSize grammar = do
    -- Enumerate frontiers
  frontiers <- mapM (\(tp, seed, _) -> do front <- enumBU frontierSize keepSize grammar tp seed
                                          forceShowHack front
                                          return front) tasks
  -- Save out the best program for each task to a file
  saveBestBU $ zip frontiers tasks
  let frontiers' = map (M.map (const 0.0)) frontiers
  let grammar' = grammarEM lambda pseudocounts (blankLibrary grammar) frontiers' --compressWeightedCorpus lambda pseudocounts grammar obs'
  let terminalLen = length $ filter isTerm $ M.keys $ grExprDistr grammar
  putStrLn $ "Got " ++ show ((length $ lines $ showGrammar $ removeSubProductions grammar') - terminalLen - 1) ++ " new productions."
  putStrLn $ "Grammar entropy: " ++ show (entropyLogDist $ M.elems $ grExprDistr grammar')
  when verbose $ putStrLn $ showGrammar $ removeSubProductions grammar'
  putStrLn "" -- newline
  return grammar'
  where saveBestBU frontiersAndTasks =
          let str = unlines $ map (\(front, (_, _, nm)) -> 
                                      let (bestProg, bestLL) = maximumBy (compare `on` snd) (M.toList front)
                                      in nm ++ "\t" ++ show bestProg ++ "\t" ++ show bestLL) frontiersAndTasks
          in writeFile prefix str