-- IterativeDeepening.hs
-- |
-- Module:      Strappy.Core.IterativeDeepening
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | This module implements Iterative Deepening A* (IDA*) for polymorphic
-- functional programs.
--
-- References
-- ----------
-- [1] R. E. Korf. Depth-first iterative-deepening: An optimal admissible tree search. Artif. Intell., 27(1):97– 109, 1985.



module Strappy.IterativeDeepening where

import qualified Data.Map as Map
import Control.Monad.Error
-- import Control.Monad.Trans.Either
import Control.Monad.List
import Control.Arrow
import Control.Monad.Identity
import Data.Monoid
import Data.String (IsString)
import Control.Monad.State
import Data.Maybe

import Strappy.Type 
import Strappy.Expr
import Strappy.Library
import Strappy.Grammar

import Numeric.StatsUtils

import Control.DeepSeq
import Criterion.Main

import Control.Exception
import System.IO.Unsafe
import Debug.Trace 

eps :: Double
eps = 1e-6

_SHOULD_LOG = False
logFile = "log.txt"
logToFile x a = if _SHOULD_LOG then unsafePerformIO $ do 
                                      appendFile logFile  (x ++ "\n")
                                      return a
                  else a

fromRight (Right x) = x
fromRight (Left err) = error $  "fromRight: " ++ err

isRight (Right _) = True
isRight _ = False

infty :: Double
infty = read "Infinity"

_DEBUG = True
debug s x = if _DEBUG then trace s x else x
traceThis s x = trace (s ++ ": " ++ show x) x
debugThis s x = if _DEBUG then traceThis s x else x


calculateLowerBound = undefined
-- lowerBound :: Grammar -> Type -> Context -> Double -> Double 
-- -- | Return a lower bound on the cost of a program of this type. 
-- -- | The lower bound is calculated recursively. Let h:Types -> Reals be the lower bound function. 
-- -- | If there is a primitive in the library of type t, then the weight of the least costly such primitive is returned. 
-- -- | Otherwise, return the cost of an application plus the sum of the lower bounds of the two application pair types. 
-- calculateLowerBound gr tp ctx@(Context nTVar subst) maxB = if null es 
--                                                     then minRecursiveCost 
--                                                     else minimum . map (\(_, w, _) -> w) $ es
--    where  es = getUnifyingExprs gr tp ctx
--           minRecursiveCost = let (tp', ctx') = freshTVar ctx
--                                  maxB' = maxB - negate (grApp gr)
--                              in negate (grApp gr) + lowerBound gr (tp' ->- tp) ctx' maxB' + lowerBound gr tp' ctx' maxB'

filterIsRight xs = go [] xs
  where go !acc [] = acc
        go !acc ((Right x):xs) = go (x:acc) xs
        go !acc ((Left _):xs) = go acc xs
        
type SearchNode = (Expr, Double, Context)
idaStar :: MonadError String m 
        => Grammar 
        -> Type   -- ^ tp: type  
        -> [Type] -- ^ ts: collection of unexpanded types elsewhere in the surrounding expression
        -> StateT Double (TypeInference m) [SearchNode]
idaStar gr tp ts = do
  lowerBound <- calculateLowerBound
  boundRemaining <- get
  if lowerBound > boundRemaining
     then return []
     else do ctx <- lift $ get -- extract current context
             -- find all unifying primitives in current context
             let leaves = map (\((a, b), c) -> (a, b, c)) . filterIsRight . map (flip runStateT ctx) $ getUnifyingPrims gr tp
             return leaves

             
             
                 
                 
  
--          -> ([(Expr, -- ^ a complete expression
--                Double, -- ^ the cost of the expressions
--                Context -- ^ the final type context of the expression
--               )],
--              Double -- ^ the remaining cost bound
--             )
-- cbSearch gr b tp ss | lowerBound > b  = ([], lb)
--                     | otherwise = loop 
--   if lb > b then([], lb)
--      else if gamma > b 
--                                       then (leaves, min cMinL gamma)
--                                       else (applications, cMinA)
--   where 
--     tp' = fst . applySub ctx $ tp
--     lowerBound = calculateLowerBound gr tp ctx b + sum [calculateLowerBound gr s ctx b | s <- ss] 
--     gamma = negate $ grApp gr
--     (leaves, cMinL) = (es, cMin')
--                     where cMin' = if null ks then infty else minimum ks
--                           es = getUnifyingPrims gr tp ctx
--                           ks = map (\(_, w, _) -> w) . filter (\(_, w, _) -> w > b) $ es
--     (applications, cMinA) = loop leaves (gamma + k_l) lhss
--           where (eta, ctx') = freshTVar ctx
--                 (lhss, k_l) = cbSearchV4 gr (b - gamma) (eta ->- tp) ctx' (eta:ss)

--                 loop out cMin [] = (out, cMin)
--                 loop out cMin ((eL, wL, ctxL):es) 
--                     | gamma + wL > b = loop out cMin es
--                     | otherwise      = loop out' cMin' es
--                     where (rhss, k_r) = cbSearchV4 gr (b - gamma - wL) eta ctxL ss
--                           (out', cMin') = innerLoop out cMin rhss
--                             where innerLoop out cMin [] = (out, cMin)
--                                   innerLoop out cMin ((eR, wR, ctxR) : es) 
--                                       | gamma + wL + wR > b = innerLoop out cMin es
--                                       | otherwise  = logToFile (show (eApp, w)) $ innerLoop ((eApp, w, ctxR):out) cMin' es
--                                           where eApp = (App eL eR tApp Nothing $ Just (negate (gamma + wR + wL)))
--                                                 w = gamma + wR + wL
--                                                 tApp = fst $ applySub ctxR eta
--                                                 cMin' = if gamma + wL + k_r > b then min cMin (gamma + wL + k_r) else cMin

-- showCbSearchResults :: ([(Expr, Double, Context)], Double) -> String
-- showCbSearchResults (out, cMin) = unlines $ 
--   ["Max value: " ++ show cMin] ++ 
--   [show e ++ ": " ++ show d | (e, d, _) <- out]

-- showSearchResults out = unlines $ [show e ++ ": " ++ show d | (e, d) <- out]


-- cbIterativeDeepening :: Grammar 
--                      -> Int
--                      -> Type 
--                      -> [(Expr, Double)]
-- cbIterativeDeepening gr n tp = go 0 
--   where (tp', ctx') = runIdentity . runStateT (Context 0 Map.empty) $ instantiateType tp

--         go b = let (res, b') = cbSearchV4 gr b tp' ctx' []
--                    k = length res
--                in if k >= n then map (\(a, b, _) -> (a, b)) res
--                             else trace ("Running Iterative Deepening with bound: " ++ show b' ++ "; num progs: " ++ show k ) $ go (b' + eps)


-- tp = tTriple (tList tChar) (tList tChar) (tList tInt) ->- (tList tInt)
-- tp' = (tList tChar) ->- (tList tChar) ->- (tList tInt) ->- (tList tInt)
-- --tp'' = tInt ->- tInt 
-- main = do
--   let res = cbIterativeDeepening seedGrammar 20 tp
--   --let (res, mv) = cbSearchV4 seedGrammar 20 tp (Context 0  Map.empty) []
--   putStr $ showSearchResults res
--   --putStr $ showCbSearchResults (res, mv)
--   putStr $ "Number of programs: " ++ show (length res)
--   --defaultMain [
--   --  --bench "cbSearchV1" $ nf (showCbSearchResults . cbSearch seedGrammar 25 tInt) (Context 0  Map.empty),
--   --  --bench "cbSearchV2" $ nf (showCbSearchResults . cbSearchV2 seedGrammar 20 tInt) (Context 0  Map.empty),
--   --  bench "cbSearchV3" $ nf (showCbSearchResults . cbSearchV3 seedGrammar 20 tp) (Context 0  Map.empty),
--   --  bench "cbSearchV4" $ nf (showCbSearchResults . cbSearchV4 seedGrammar 20 tp (Context 0  Map.empty)) []
--   --  ]

