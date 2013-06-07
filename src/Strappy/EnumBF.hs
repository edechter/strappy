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


data Turn = L | R deriving (Show, Eq)
type Path = [Turn] 

-- | An ADT corresponding to a ``solution base.'' See Pearl Heuristics
-- book Chapter 2.
data CombBase = CombBase {comb :: Expr, 
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
                        closed :: MaxQueue CombBase, 
                        bfHistory :: Set.Set Expr }

isClosed :: CombBase -> Bool
isClosed CombBase{ path = Nothing }  = True
isClosed _ = False

-- | Inner nodes are represented by Terminals with ?'s for names
-- This is done so that we can use the existing Expr gadt without defining our own new structure
cInnerNode tp = Term { eName = "?", eType = tp, eThing = undefined,
                       eReqType = Nothing }

enumBF :: Grammar 
       -> Int -- max num combinators
       -> Type
       -> [(Expr, Double)]
-- | Breadth-first AO enumeration of combinators with highest scores
-- under the grammar.
enumBF gr i tp = map (\cb -> (comb cb, value cb)) $ PQ.take i $ closed $ enumBF' gr' i initBFState
    where root = CombBase (cInnerNode tp) (Just []) tiState 0.0
          tiState = execState (initializeTI $ grExprDistr gr) (0, Map.empty)
          initBFState = BFState (PQ.singleton root) PQ.empty Set.empty
          gr' = normalizeGrammar gr

enumBF' :: Grammar -> Int -> BFState -> BFState
-- | Removes and expands the maximal entry in the OPEN list. The
-- resulting elements are added either to the open list or to the
-- closed list. This process is repeated until there are at least i
-- elements in closed list. 
enumBF' _  _ bfState@(BFState { open = openPQ }) | PQ.size openPQ == 0 = bfState
enumBF' gr i bfState@(BFState openPQ closedPQ hist) = 
    if PQ.size closedPQ >= i then bfState else
        let (cb, openPQ') = PQ.deleteFindMax openPQ -- get best open solution
        in if isClosed cb
           then if Set.member (comb cb) hist
                then enumBF' gr i $ BFState openPQ' closedPQ hist
                else let expr = annotateRequested' (eType $ comb cb) $ comb cb
                         closedPQ' = PQ.insert (cb { comb = expr,
                                                     value = exprLogLikelihood gr expr }) closedPQ
                         hist' = Set.insert expr hist
                     in enumBF' gr i $ BFState openPQ' closedPQ' hist'
           else let cbs = expand gr cb
                    openPQ'' = PQ.fromList cbs `PQ.union` openPQ'
                in enumBF' gr i $ BFState openPQ'' closedPQ hist

expand :: Grammar 
       -> CombBase 
       -> [CombBase]
expand gr cb@(CombBase (Term { eType = tp }) (Just []) ti v) =
  let tp' = runIdentity $ evalStateT (applySub tp) ti
      logTerm = log (1 - exp (grApp gr))
      cs = filter (\(e, _) -> canUnifyFast tp' (eType e)) $ Map.toList $ grExprDistr gr
      cbs = map (\(e, ll) -> CombBase e Nothing (ti' e) (v+ll+logTerm)) cs
      ti' e = execState (instantiateType (eType e) >>= unify tp) ti
      cbApp = expandToApp gr cb
  in if null cs then [] else cbApp : cbs -- Do things break horribly if we don't have this conditional?
expand gr (CombBase expr@(App { eLeft = left }) (Just (L:rest)) ti v) = do
  (CombBase left' rest' ti' v') <- expand gr $ CombBase left (Just rest) ti v
  let path' =
        case rest' of
          Nothing -> Just [R]
          Just ys -> Just (L:ys)
  let expr' = expr { eLeft = left' }
  return $ CombBase expr' path' ti' v'
expand gr (CombBase expr@(App { eRight = right }) (Just (R:rest)) ti v) = do
  (CombBase right' rest' ti' v') <- expand gr $ CombBase right (Just rest) ti v
  let path' =
        case rest' of
          Nothing -> Nothing
          Just ys -> Just (R:ys)
  let expr' = expr { eRight = right' }
  return $ CombBase expr' path' ti' v'

expandToApp :: Grammar -> CombBase -> CombBase
expandToApp gr (CombBase (Term { eType = tp }) (Just []) ti v) =
  let ((lTp, rTp), ti') = runIdentity $ Prelude.flip runStateT ti $ do
        rightType <- mkTVar
        leftType <- applySub $ rightType ->- tp
        return (leftType, rightType)
      expr = App { eLeft = cInnerNode lTp,
                   eRight = cInnerNode rTp,
                   eType = tp,
                   eReqType = Nothing }
  in CombBase expr (Just [L]) ti' (v + grApp gr)


