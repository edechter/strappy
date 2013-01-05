-- EnumBF

import Data.Function (on)
import Control.Monad.State

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
                          path :: Path, -- ^ a path to the current node of interest
                          isGoal :: Bool, -- ^ does comb contain nonterminals
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

expand :: Grammar 
       -> CombBase 
       -> StateT Int [] CombBase
expand gr (CombBase c (R:rest) v) = 
expand gr (CombBase c (L:rest) v) = undefined
expand gr (CombBase (CTerminal t) [] v = undefined

