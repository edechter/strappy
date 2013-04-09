-- EnumBF

module Strappy.EnumBF where 

import Data.Function (on)
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe
import Data.PQueue.Max (MaxQueue)
import qualified Data.PQueue.Max as PQ
import Debug.Trace 

import Strappy.Type
import Strappy.CL hiding (R, L)
import Strappy.StdLib
import Strappy.Expr
import Strappy.CLError
import Strappy.CombMap (CombMap, (!))
import qualified Strappy.CombMap as CM
import Strappy.Grammar


data Turn = L | R deriving (Show, Eq)
type Path = [Turn] 

-- | An ADT corresponding to a ``solution base.'' See Pearl Heuristics
-- book Chapter 2.
data CombBase = CombBase {comb :: Comb, 
                          path :: Maybe Path, -- ^ a path to the
                                              -- current node of
                                              -- interest
                          value :: Double
                         } deriving (Show, Eq)

instance Ord CombBase where
    compare = compare `on` value

data CombBaseTuple = CombBaseTuple { combBase :: CombBase, 
                                     tpVar :: Int} deriving (Show, Eq)

instance Ord CombBaseTuple where
    compare = compare `on` combBase              

data BFState = BFState {open :: MaxQueue CombBaseTuple, 
                        closed :: MaxQueue CombBaseTuple} deriving Show

isClosed :: CombBase -> Bool
isClosed cb | path cb == Nothing  = True
            | otherwise           = False

-- hack min grammar val
hackMinGrammarVal = log (0.25)


enumBF :: Grammar 
       -> Int -- max num combinators
       -> Type
       -> [CombBase]
-- | Breadth-first AO enumeration of combinators with highest scores
-- under the grammar.
enumBF gr i tp = map combBase (PQ.take i $ closed $ enumBF' gr i initBFState)
    where root = CombBaseTuple (CombBase (CInnerNode tp) (Just []) 0) 0
          initBFState = BFState (PQ.singleton root) PQ.empty

enumBF' :: Grammar -> Int -> BFState -> BFState
-- | Removes and expands the maximal entry in the OPEN list. The
-- resulting elements are added either to the open list or to the
-- closed list. This process is repeated until there are at least i
-- elements in closed list. 
enumBF' gr i bfState@(BFState openPQ closedPQ) | PQ.size openPQ == 0 = bfState
enumBF' gr i bfState@(BFState openPQ closedPQ) = 
    if PQ.size closedPQ >= i then bfState else
        let (cbt, openPQ') = PQ.deleteFindMax openPQ -- get best open solution
            cbs =  map (\(c,i) -> CombBaseTuple c i) 
                  $ runStateT (expand gr (combBase cbt)) (tpVar cbt)
            closedCBs = filter (isClosed . combBase) cbs
            openCBs= filter (not . isClosed . combBase) cbs
            closedPQ' = (PQ.fromList closedCBs) `PQ.union` closedPQ
            openPQ'' = PQ.fromList openCBs `PQ.union` openPQ'
        in (trace $ "OPEN: " ++ (show $ PQ.size openPQ'')
            ++ " CLOSED: " ++ (show $ PQ.size closedPQ'))
           $ enumBF' gr i $ BFState openPQ'' closedPQ'
        

expandToApp :: Grammar -> CombBase -> StateT Int [] CombBase
expandToApp gr (CombBase (CInnerNode tp) (Just []) v) = 
    do tp' <- newTVar Star
       let t_left = tp' ->- tp
           t_right = tp'
           c_left = (CInnerNode t_left) 
           c_right = (CInnerNode t_right)
           c = CApp c_left c_right tp 1 Nothing
           minGrammarVal = hackMinGrammarVal
--           minGrammarVal = minimum (CM.elems (library gr))
           cb = CombBase c (Just [L]) (2*minGrammarVal)
       return cb

expand :: Grammar 
       -> CombBase 
       -> StateT Int [] CombBase
expand gr cb@(CombBase (CInnerNode tp) (Just []) v) 
    = if cs_is_empty then (trace $ "EMPTY") $ mzero else 
          cbs `mplus` cbApp
    where primCombs = CM.keys (library gr)
          cs = filterCombinatorsByType primCombs tp
--          minGrammarVal = minimum (CM.elems (library gr))
          minGrammarVal = hackMinGrammarVal
          cbs =   do c <- cs
                     let value = (calcLogProb gr tp c) + minGrammarVal
                         combBase = CombBase c Nothing value
                     return combBase
          cs_is_empty =  if not $ isTAp tp then False else
                             not $ existCombWithToType primCombs (toType tp)
          cbApp = expandToApp gr cb 

expand gr cb@(CombBase (CApp c_left c_right tp d _) (Just (L:rest)) v) = 
    let --minGrammarVal = minimum (CM.elems (library gr)) in
        minGrammarVal = hackMinGrammarVal in
    do (CombBase c_left' rest' v')  <- expand gr 
                                       $ CombBase c_left 
                                             (Just rest) 
                                             (v - minGrammarVal)
       let tp_right' = fromType (cType c_left')
           c_right' =  CInnerNode tp_right'

       let path' = case rest' of
                     Nothing -> Just [R]
                     Just ys -> Just (L:ys)

       let tp' = toType (cType c_left')
           d' = if (cDepth c_left') + 1 > d then cDepth c_left' + 1 else d

       return $ CombBase (CApp c_left' c_right' tp' d' Nothing) path' 
                  (v' + v - minGrammarVal)

expand gr cb@(CombBase (CApp c_left c_right tp d _) (Just (R:rest)) v) = 
    let --minGrammarVal = minimum (CM.elems (library gr)) in
        minGrammarVal = hackMinGrammarVal in
    do (CombBase c_right' rest' v')  <- expand gr 
                                       $ CombBase c_right 
                                             (Just rest) 
                                             v 
       t <- newTVar Star
       let backsub = fromJust $ mgu (cType c_left) (cType c_right' ->- t)
           tp' = toType (apply backsub (cType c_left))
       let path' = case rest' of
                     Nothing -> Nothing
                     Just ys -> Just (R:ys)
           d' = if (cDepth c_right') + 1 > d then cDepth c_right' + 1 else d
       return $ CombBase (CApp c_left c_right' tp' d' Nothing) path' (v' + v - minGrammarVal)



doTypesUnify :: Type -> Type -> Bool
doTypesUnify t1 t2 = case mgu t1 t2 of
                       Just _ -> True
                       Nothing -> False

doesCombHaveToType :: Comb -> Type -> State Int Bool
doesCombHaveToType c t = do ct <- freshInst (cType c)
                            if doTypesUnify ct t then 
                                return True else
                                case ct of 
                                  (TAp ct1 ct2) -> return $ doTypesUnify ct2 t
                                  _ -> return False

existCombWithToType :: [Comb] -> Type -> Bool
existCombWithToType cs t = any (==True) $ fst $ runState m 0
    where m = sequence $ do c <- cs
                            return $ doesCombHaveToType c t

          


