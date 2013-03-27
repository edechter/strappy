-- TestHashMap.hs

import Data.HashMap (Map, lookup, insert, empty)
import Data.Hashable

data A = A [Int] deriving (Eq, Ord)

g :: A -> Int
g (A (x:xs))| x == 1    = 1
            | otherwise = 0

c :: Map A Int
c = empty

f :: A 
  -> (A -> Int) -- g
  -> (Map A Int) -- c
  -> Int
func (A (x:xs)) = (g xs) +  (f xs)