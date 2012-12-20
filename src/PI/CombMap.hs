-- CombMap.hs

module CombMap where

import qualified Data.HashMap as HMap
import Data.Hashable
import qualified Data.Map as Map

import CL
import StdLib 

type CombMap a = HMap.Map Comb a

instance Hashable Comb where
    hash (CNode name _ _) = hash name
    hash (CApp rComb lComb _ _) = hash rComb `hashWithSalt` lComb
    


lib = Map.elems stdlib
c = cI
x = HMap.fromList [(cI, 2)]