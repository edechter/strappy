{-# Language ExistentialQuantification #-}

-- EnumBF


module Strappy.EnumBF where 

import Data.Function (on)
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.HashMap as HashMap
import Data.PQueue.Max (MaxQueue)
import qualified Data.PQueue.Max as PQ
import Debug.Trace 

import Strappy.Type
import Strappy.Expr
import Strappy.Library


data Turn = L | R deriving (Show, Eq)
type Path = [Turn] 

-- | An ADT corresponding to a ``solution base.'' See Pearl Heuristics
-- book Chapter 2.
data CombBase = forall a. CombBase {comb :: Expr a, 
                                    path :: Maybe Path, -- ^ a path to the
                                                        -- current node of
                                                        -- interest
                                    inferState :: (Int, Map.Map Int Type),
                                    value :: Double
                                   }

-- Existential type prevents us from deriving instances
instance Eq CombBase where
  (==) (CombBase{ comb = a}) (CombBase{ comb = b}) = show a == show b

instance Ord CombBase where
    compare = compare `on` value

data BFState = BFState {open :: MaxQueue CombBase, 
                        closed :: MaxQueue CombBase }

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
       -> [CombBase]
-- | Breadth-first AO enumeration of combinators with highest scores
-- under the grammar.
enumBF gr i tp = PQ.take i $ closed $ enumBF' gr' i initBFState
    where root = CombBase (cInnerNode tp) (Just []) tiState 0.0
          tiState = execState (initializeTI $ grExprDistr gr) (0, Map.empty)
          initBFState = BFState (PQ.singleton root) PQ.empty
          gr' = normalizeGrammar gr

enumBF' :: Grammar -> Int -> BFState -> BFState
-- | Removes and expands the maximal entry in the OPEN list. The
-- resulting elements are added either to the open list or to the
-- closed list. This process is repeated until there are at least i
-- elements in closed list. 
enumBF' gr i bfState@(BFState openPQ closedPQ) | PQ.size openPQ == 0 = bfState
enumBF' gr i bfState@(BFState openPQ closedPQ) = 
    if PQ.size closedPQ >= i then bfState else
        let (cb, openPQ') = PQ.deleteFindMax openPQ -- get best open solution
        in if isClosed cb
           then enumBF' gr i $ BFState openPQ' $ PQ.insert cb closedPQ
           else let cbs = expand gr cb
                    openPQ'' = PQ.fromList cbs `PQ.union` openPQ'
                in enumBF' gr i $ BFState openPQ'' closedPQ

expand :: Grammar 
       -> CombBase 
       -> [CombBase]
expand gr cb@(CombBase (Term { eType = tp }) (Just []) ti v) =
  let cs = filter (\(e, _) -> canUnify tp (eType $ fromUExpr e)) $ HashMap.toList $ grExprDistr gr
      cbs = map (\(e, ll) -> CombBase (fromUExpr e) Nothing (ti' e) (v+ll)) cs
      ti' e = execState (instantiateType (eType $ fromUExpr e) >>= unify tp) ti
      cbApp = expandToApp gr cb
  in if null cs then [] else cbApp : cbs
expand gr (CombBase (App { eLeft = left, eRight = right, eType = tp }) (Just (L:rest)) ti v) = do
  (CombBase left' rest' ti' v') <- expand gr $ CombBase left (Just rest) ti v
  let path' =
        case rest' of
          Nothing -> Just [R]
          Just ys -> Just (L:ys)
  let expr = App { eLeft = fromUExpr $ toUExpr left',
                   eRight = right,
                   eType = tp,
                   eReqType = Nothing,
                   eLogLikelihood = Nothing,
                   eLabel = Nothing }
  return $ CombBase expr path' ti' v'
expand gr (CombBase (App { eLeft = left, eRight = right, eType = tp }) (Just (R:rest)) ti v) = do
  (CombBase right' rest' ti' v') <- expand gr $ CombBase right (Just rest) ti v
  let path' =
        case rest' of
          Nothing -> Nothing
          Just ys -> Just (R:ys)
  let expr = App { eRight = fromUExpr $ toUExpr right',
                   eLeft = left,
                   eType = tp,
                   eReqType = Nothing,
                   eLogLikelihood = Nothing,
                   eLabel = Nothing }
  return $ CombBase expr path' ti' v'

expandToApp :: Grammar -> CombBase -> CombBase
expandToApp gr (CombBase (Term { eType = tp }) (Just []) ti v) =
  let ((lTp, rTp), ti') = runIdentity $ flip runStateT ti $ do
        rightType <- mkTVar
        leftType <- applySub $ rightType ->- tp
        return (leftType, rightType)
      expr = App { eLeft = cInnerNode lTp,
                   eRight = cInnerNode rTp,
                   eType = tp, -- type is actually irrelevant
                   eReqType = Nothing,
                   eLogLikelihood = Nothing,
                   eLabel = Nothing }
  in CombBase expr (Just [L]) ti' (v + grApp gr)


