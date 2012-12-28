-- CompressionSearch.hs

module CompressionSearch where

import Control.Monad.State
import Data.List (sortBy)
import Infinite

import CL
import qualified CombMap as CM
import qualified Compress as CP (getUniqueTrees)
import CombMap (CombMap)
import Task


    
data TaskVar = TaskVar {nodeTask  ::  Task,
                        nodeCombs :: [Comb]} deriving Show

-- data SearchNode = SearchNode { 
--       index :: CombMap Int, -- current index
--       solution :: [Comb], -- current solution
--       g :: Infinite Int, -- node cost 
--       h :: Infinite Int -- current upperbound on best cost
--     } deriving Show

nullIndex :: CombMap Int
nullIndex = CM.empty

updateIndex :: CombMap Int -> Comb -> CombMap Int
updateIndex index c = CM.unionWith (+) index (CP.getUniqueTrees c)

mkIndex :: [Comb] -> CombMap Int
mkIndex = foldl updateIndex CM.empty  

-- dfs :: SearchNode -> [[Comb]] -> [Comb]
-- dfs n ((c:cs):rest) = let ind' = updateIndex (index n) c
--                           g' = (Only . length . CM.keys) ind'

-- | DFS. Take a list of combinators and the current index. G = length
-- of the index. Return G* the minimal cost of the minimal cost
-- solution in that subtree. 
-- 
-- Algorithm: set h to infinity. Get G* of next child. Add to
-- edge weight. That's the new h of the top node. Continue.  
                          
                          
                          
                     
               
                  





                 







                 
                 
  












