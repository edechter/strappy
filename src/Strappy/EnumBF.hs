{-# Language ExistentialQuantification #-}

-- EnumBF


module Strappy.EnumBF where 

import Data.Function (on)
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
--import qualified Data.HashMap as HashMap
import Data.PQueue.Max (MaxQueue)
import qualified Data.PQueue.Max as PQ

import Strappy.Utils
import Strappy.Type
import Strappy.Expr
import Strappy.Library
import Strappy.Config


data Turn = L | R deriving (Show, Eq)
type Path = [Turn] 

-- | An ADT corresponding to a ``solution base.'' See Pearl Heuristics
-- book Chapter 2.
data CombBase = CombBase {comb :: Expr, 
                          path :: Maybe Path, -- ^ a path to the
                                              -- current node of
                                              -- interest
                          numUnexpanded :: Int, -- ^ number of non-expanded terminals
                          inferState :: (Int, Map.Map Int Type),
                          value :: Double
                         }
-- | The search now uses an admissible heuristic:
-- any unexpanded leaf is assumed to have the LL of the most probable production,
-- plus the cost of choosing a production
-- This way, we penalize search nodes with many unexpanded leaves,
-- while still finding the top N most probable nodes.

-- Existential type prevents us from deriving instances
instance Eq CombBase where
  (==) (CombBase{ comb = a}) (CombBase{ comb = b}) = show a == show b

instance Ord CombBase where
    compare = compare `on` value

data BFState = BFState {open :: MaxQueue CombBase, 
                        closed :: MaxQueue CombBase, 
                        bfHistory :: Set.Set Expr }

isClosed :: CombBase -> Bool
isClosed CombBase{ path = Nothing }  = True
isClosed _ = False

-- | Inner nodes are represented by Terminals with ?'s for names
-- This is done so that we can use the existing Expr gadt without defining our own new structure
cInnerNode tp = Term { eName = "?", eType = tp, eThing = undefined,
                       eReqType = Nothing, eLogLikelihood = Nothing }

enumBF :: Grammar 
       -> Int -- max num combinators
       -> Type
       -> [(Expr, Double)]
-- | Breadth-first AO enumeration of combinators with highest scores
-- under the grammar.
enumBF gr i tp = map (\cb -> (comb cb, value cb)) $ PQ.take i $ closed $ enumBF' gr' bestLeaf i initBFState
    where root = CombBase (cInnerNode tp) (Just []) 1 tiState bestLeaf
          tiState = snd $ runTiInIdentity (initializeTI $ grExprDistr gr)
          initBFState = BFState (PQ.singleton root) PQ.empty Set.empty
          gr' = normalizeGrammar gr
          bestLeaf = log (1 - exp (grApp gr)) + (maximum $ Map.elems $ grExprDistr gr)

enumBF' :: Grammar -> Double -> Int -> BFState -> BFState
-- | Removes and expands the maximal entry in the OPEN list. The
-- resulting elements are added either to the open list or to the
-- closed list. This process is repeated until there are at least i
-- elements in closed list. 
enumBF' _ _ _ bfState@(BFState { open = openPQ }) | PQ.size openPQ == 0 = bfState
enumBF' gr bestLeaf i bfState@(BFState openPQ closedPQ hist) = 
    if PQ.size closedPQ >= i then bfState else
        let (cb, openPQ') = PQ.deleteFindMax openPQ -- get best open solution
        in if isClosed cb
           then if Set.member (comb cb) hist
                then enumBF' gr bestLeaf i $ BFState openPQ' closedPQ hist
                else let expr = exprLogLikelihood gr $ annotateRequested' (eType $ comb cb) $ comb cb
                         closedPQ' = PQ.insert (cb { comb = expr,
                                                     value = fromJust $ eLogLikelihood expr }) closedPQ
                         hist' = Set.insert expr hist
                     in enumBF' gr bestLeaf i $ BFState openPQ' closedPQ' hist'
           else let cbs = expand gr bestLeaf cb
                    openPQ'' = PQ.fromList cbs `PQ.union` openPQ'
                in enumBF' gr bestLeaf i $ BFState openPQ'' closedPQ hist

expand :: Grammar
       -> Double
       -> CombBase 
       -> [CombBase]
expand gr bestLeaf cb@(CombBase (Term { eType = tp }) (Just []) unexpanded (nextTVar, sub) v) =
  let (tp', (nextTVar', sub')) = runTIWithNextVarAndSubInIdentity nextTVar sub (applySub tp) 
      logTerm = log (1 - exp (grApp gr))
      cs = filter (\(e, _) -> canUnifyFast tp' (eType e)) $ Map.toList $ grExprDistr gr
      z = {-if usePCFGWeighting then 0.0 else-} logSumExpList $ map snd cs
      cbs = map (\(e, ll) -> CombBase e Nothing (unexpanded - 1) (ti' e) (v+ll+logTerm-z-bestLeaf)) cs
      ti' e = snd $ runTIWithNextVarAndSubInIdentity nextTVar' sub' (instantiateType (eType e) >>= unify tp) 
      cbApp = expandToApp gr bestLeaf cb
  in if null cs then [] else cbApp : cbs
expand gr bestLeaf (CombBase expr@(App { eLeft = left }) (Just (L:rest)) unexpanded ti v) = do
  (CombBase left' rest' unexpanded' ti' v') <- expand gr bestLeaf $ CombBase left (Just rest) unexpanded ti v
  let path' =
        case rest' of
          Nothing -> Just [R]
          Just ys -> Just (L:ys)
  let expr' = expr { eLeft = left' }
  return $ CombBase expr' path' unexpanded' ti' v'
expand gr bestLeaf (CombBase expr@(App { eRight = right }) (Just (R:rest)) unexpanded ti v) = do
  (CombBase right' rest' unexpanded' ti' v') <- expand gr bestLeaf $ CombBase right (Just rest) unexpanded ti v
  let path' =
        case rest' of
          Nothing -> Nothing
          Just ys -> Just (R:ys)
  let expr' = expr { eRight = right' }
  return $ CombBase expr' path' unexpanded' ti' v'

expandToApp :: Grammar -> Double -> CombBase -> CombBase
expandToApp gr bestLeaf (CombBase (Term { eType = tp }) (Just []) unexpanded (nextTVar, sub) v) =
  let ((lTp, rTp), ti') = runTIWithNextVarAndSubInIdentity nextTVar sub$ do
        rightType <- mkTVar
        leftType <- applySub $ rightType ->- tp
        return (leftType, rightType)
      expr = App { eLeft = cInnerNode lTp,
                   eRight = cInnerNode rTp,
                   eType = tp,
                   eReqType = Nothing, eLogLikelihood = Nothing }
  in CombBase expr (Just [L]) (unexpanded+1) ti' (v + grApp gr + bestLeaf)


