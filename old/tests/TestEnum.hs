-- TestEnum.hs

import Control.Monad.State
import Data.Maybe

import Enumerate
import Type
import CL
import StdLib
import qualified CombMap as CM

t30 = mkTVar 30
t40 = mkTVar 40

-- target = tInt
-- t = t1 ->- t0 ->- tInt
-- t_left0 = t30 ->- t
-- left = fst $ head $ runStateT (filterCombinatorsByType (CM.keys stdlib) t_left0) 100
-- t_left1 = cType left
-- t_right0 = fromType t_left1
-- right = fst $ head $ runStateT (filterCombinatorsByType (CM.keys stdlib) t_right0) 100
-- t_right1 = cType right
-- backsub = fromJust $ mgu t_left1 (t_right1 ->- t40)
-- t_root = toType (apply backsub t_left1)
-- combined = CApp left right t_root (mkAppDepth left right)

target = t0 ->- tInt
left = dOp2C "+" (+)
t_left1 = (tInt ->- tInt) ->- tInt ->- tInt
t_right0 = fromType t_left1
right = fst $ head $ runStateT (filterCombinatorsByType (CM.keys stdlib) t_right0) 100



