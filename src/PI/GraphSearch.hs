-- GraphSearch.hs
{-# Language MultiParamTypeClasses, FunctionalDependencies #-}
-- | This module provides functions for searching for a set of
-- assignments to variables that minimize some cost.

module GraphSearch where

import qualified Data.List as List
import Control.Monad.State
import Data.Maybe
import Debug.Trace

import Infinite

data SearchNode a = SearchNode { partialAssignment :: [a],
                                 currentValue :: Maybe a,
                                 bestExtension :: Maybe [a],
                                 h :: Infinite Int
                                 } deriving Show


-- | Goal: return a search node. The complete problem solution is when
-- the the partial assignment is empty, the best extension si the
-- length of all the variables,
dfs :: (Show a ) => 
       SearchNode a -- partial assignment
    -> (SearchNode a -> a -> Infinite Int) -- edge weight cost
    -> [[a]] -- other assignments
    -> SearchNode a -- search node with current best extension
dfs sn w (values:rest) 
    =  foldl minWShortCircuit sn values

      where minWShortCircuit sn@(SearchNode pa cv be _) v 
                = let edgeCost = w sn v
                      cost = edgeCost + h childSn
                      childSn = dfs (expandWith sn v) w rest
                      newSn = updateSnWith sn childSn cost
                  in if edgeCost < h sn && cost < h sn
                     then newSn
                     else sn

            expandWith sn@(SearchNode pa cv be _) v 
                = SearchNode (pa ++ [v]) Nothing Nothing PosInfinity

            updateSnWith sn@(SearchNode pa cv be _) 
                         chidlSn@(SearchNode pa' cv' be' _) cost
                             = sn{currentValue = Just $ last pa', 
                                  bestExtension = append cv' be',
                                  h = cost}
                               
            append (Just a) (Just bs) = Just (a:bs)
            append Nothing (Just bs) = Just bs
            append Nothing Nothing = Nothing
            append (Just a) Nothing = Nothing

dfs sn _ [] = sn { bestExtension = Just [], h = Only 0}



                    
                


                               

             
    
                           


