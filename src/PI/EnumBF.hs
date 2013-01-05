-- EnumBF

import Data.Function (on)
import Control.Monad.State
import Data.Maybe

import Type
import CL
import StdLib
import Expr
import CLError
import CombMap (CombMap, (!))
import qualified CombMap as CM
import Grammar


data Turn = L | R deriving (Show, Eq)
type Path = [Turn] 

-- | An ADT corresponding to a ``solution base.'' See Pearl Heuristics
-- book Chapter 2.
data CombBase = CombBase {comb :: Comb, 
                          path :: Maybe Path, -- ^ a path to the
                                              -- current node of
                                              -- interest
                          value :: Int
                         } deriving (Show, Eq)

instance Ord CombBase where
    compare = compare `on` value


              

enumBF :: Grammar 
       -> Int -- max num combinators
       -> Type
       -> StateT Int [] Comb
-- | Bread-first AO enumeration of combinators with highest scores
-- under the grammar.
enumBF = undefined

expandToApp :: Grammar -> CombBase -> StateT Int [] CombBase
expandToApp gr (CombBase (CTerminal tp) (Just []) v) = 
    do tp' <- newTVar Star
       let t_left = tp' ->- tp
           t_right = tp'
           c_left = (CTerminal t_left) 
           c_right = (CTerminal t_right)
           c = CApp c_left c_right tp 1
           cb = CombBase c (Just [L]) (expansions gr)
       return cb


expand :: Grammar 
       -> CombBase 
       -> StateT Int [] CombBase
expand gr cb@(CombBase (CTerminal tp) (Just []) v) = cbs `mplus` cbApp
    where primCombs = CM.keys (library gr)
          cs = filterCombinatorsByType primCombs tp
          cbs = do c <- cs
                   let value = (library gr) CM.! c
                       combBase = CombBase c Nothing (v + value)
                   return combBase
          cbApp = expandToApp gr cb 

expand gr cb@(CombBase (CApp c_left c_right tp d) (Just (L:rest)) v) = 
    do (CombBase c_left' rest' v')  <- expand gr 
                                       $ CombBase c_left 
                                             (Just rest) 
                                             (v - expansions gr)
       let tp_right' = fromType (cType c_left')
           c_right' = CTerminal tp_right'

       let path' = case rest' of
                     Nothing -> Just [R]
                     Just ys -> Just (L:ys)

       let tp' = toType (cType c_left')
           d' = if (cDepth c_left') + 1 > d then cDepth c_left' + 1 else d
       return $ CombBase (CApp c_left' c_right' tp' d') path' (v' + expansions gr)

expand gr cb@(CombBase (CApp c_left c_right tp d) (Just (R:rest)) v) = 
    do (CombBase c_right' rest' v')  <- expand gr 
                                       $ CombBase c_right 
                                             (Just rest) 
                                             0
       t <- newTVar Star
       let backsub = fromJust $ mgu (cType c_left) (cType c_right ->- t)
           tp' = toType (apply backsub (cType c_left))
       let path' = case rest' of
                     Nothing -> Nothing
                     Just ys -> Just (R:ys)
           d' = if (cDepth c_right') + 1 > d then cDepth c_right' + 1 else d
       return $ CombBase (CApp c_left c_right' tp' d') path' (v' + v)




        




