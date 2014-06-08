
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
import Control.Monad.Random
import System.Random

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

mcmcNeighbors :: Expr ->
                 [(Expr, [Expr])] -> -- ^ Rewrites
                 [Expr] -- ^ Possible moves
mcmcNeighbors expr rewrites =
  filter typeChecks $ nub $ invertRewrites expr rewrites ++ reductionNeighbors expr

mcmcMove :: Grammar -> [(Expr, [Expr])] -> Expr -> IO Expr
mcmcMove g rewrites e = do
  let ns = mcmcNeighbors e rewrites
  idx <- getRandomR (0,length ns - 1)
  let child = ns !! idx
  let childNeighbors = mcmcNeighbors child rewrites
  let toplevelRequest = fromJust $ eReqType e
  let child' = exprLogLikelihood g $ annotateRequested' toplevelRequest child
  let childLL = fromJust $ eLogLikelihood child'
  let myLL = fromJust $ eLogLikelihood e
  let logAlpha = childLL - myLL + log (genericLength childNeighbors) - log (genericLength ns)
  let alpha = exp logAlpha
  if alpha > 1
  then return child'
  else do x <- randomIO
          if alpha > x
          then return child'
          else return e

buMCMC :: Grammar ->
          Type ->
          Int ->
          Expr ->
          IO (M.Map Expr Double)
buMCMC gr tp iters e = do
  let e' = exprLogLikelihood gr $ annotateRequested' tp e
  cnts <- loop iters e' M.empty
  return $ M.map (\cnt -> log $ fromIntegral cnt / fromIntegral iters) cnts
  where templates = appendTemplates ++ concatMap getTemplates (map fst $ M.toList $ grExprDistr gr)
        loop 0 _ m = return m
        loop i e' m = do e'' <- mcmcMove gr templates e'
                         let m' = M.insertWith (+) e'' 1 m
                         loop (i-1) e'' m'

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
  frontiers <- mapM (\(tp, seed, nm) -> do front <- if sampleByEnumeration
                                                    then enumBU frontierSize keepSize grammar tp seed
                                                    else buMCMC grammar tp frontierSize seed
                                           forceShowHack front
                                           putStrLn $ "Got " ++ show (M.size front) ++ " programs for " ++ nm
                                           return front) tasks
  -- Save out the best program for each task to a file
  saveBestBU $ zip frontiers tasks
  let frontiers' = map (\fnt -> M.map (const 0.0) fnt) frontiers
  let obs = foldl (\acc frontier ->
                    M.unionWith logSumExp acc frontier) M.empty frontiers
  -- Exponentiate log likelihoods to get final weights
  let obs' = map (\(e,logW) -> (e, exp logW)) $
             M.toList obs
  let grammar' = compressWeightedCorpus lambda pseudocounts grammar obs'
  --let grammar' = grammarHillClimb lambda pseudocounts (blankLibrary grammar) frontiers'
  --let grammar' = grammarEM lambda pseudocounts (blankLibrary grammar) frontiers' --compressWeightedCorpus lambda pseudocounts grammar obs'
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