-- GraphSearch.hs
{-# Language MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
-- | This module provides functions for searching for a set of
-- assignments to variables that minimize some cost.

module GraphSearch where

import qualified Data.List as List
import Control.Monad.State
import Data.Maybe

import qualified Data.HashMap as HM
import qualified Data.Hashable as HS

import Debug.Trace

import Infinite

data SearchNode a b = SearchNode { partialAssignment :: [a],
                                   currentValue :: Maybe a,
                                   bestExtension :: Maybe [a],
                                   h :: Infinite Int,
                                   nodeInfo :: b
                                 } deriving Show

getAssignment :: SearchNode a b -> Maybe [a]
getAssignment (SearchNode pa cv be _ _) = let concat = liftM2 (++)
                                              prepend = liftM2 (:)
                                          in (Just pa) `concat` (cv `prepend` be)


-- | Goal: return a search node. The complete problem solution is when
-- the the partial assignment is empty, the best extension is the
-- length of all the variables,
dfs :: (Show a ) => 
       SearchNode a b -- partial assignment
    -> (SearchNode a b -> a -> (b, Infinite Int)) -- edge weight cost
    -> [[a]] -- other assignments
    -> SearchNode a b -- search node with current best extension
dfs sn w (values:rest) 
    =  (trace $ show $ length values) $ foldl minWShortCircuit sn values

      where minWShortCircuit sn@(SearchNode pa cv be _ _) v 
                = let (ni', edgeCost) = w sn v
                      cost = edgeCost + h childSn
                      childSn = dfs (expandWith sn v ni') w rest
                      newSn = updateSnWith sn childSn cost
                  in if edgeCost < h sn && cost < h sn
                     then newSn
                     else sn

            expandWith sn@(SearchNode pa cv be _ _) v ni
                = SearchNode (pa ++ [v]) Nothing Nothing PosInfinity ni

            updateSnWith sn@(SearchNode pa cv be _ _) 
                         chidlSn@(SearchNode pa' cv' be' _ _ ) cost
                             = sn{currentValue = Just $ last pa', 
                                  bestExtension = append cv' be',
                                  h = cost}
                               
            append (Just a) (Just bs) = Just (a:bs)
            append Nothing (Just bs) = Just bs
            append Nothing Nothing = Nothing
            append (Just a) Nothing = Nothing

dfs sn _ [] = sn { bestExtension = Just [], h = Only 0}


type SearchCache a b = HM.Map (SearchNode a b) (SearchNode a b)
dfsWCache :: (Show a, Show b, Ord (SearchNode a b), HS.Hashable (SearchNode a b )) => 
             SearchNode a b -- partial assignment
          -> SearchCache a b -- current cache of subcomputations
          -> (SearchNode a b -> a -> (b, Infinite Int)) -- edge weight cost
          -> [[a]] -- other assignments
          -> (SearchNode a b, SearchCache a b) -- search node with current best extension
dfsWCache sn cache w (values:rest) 
    = case HM.lookup sn cache of 
        Nothing -> (trace $ "No Cache: " ++ showSn sn ++ showCache cache) 
                   $ let (sn', cache') = foldl minWShortCircuit (sn, cache) values
                         cache'' = HM.insert sn' sn' cache'
                     in (sn', cache'')
        Just sn' -> (trace $ "Cache: " ++ showSn sn ++ showCache cache) $ (sn', cache)
      where minWShortCircuit (sn@(SearchNode pa cv be _ _), cache) v 
                = let (ni', edgeCost) = w sn v
                      cost = edgeCost + h childSn
                      (childSn, cache') = dfsWCache (expandWith sn v ni') cache w rest
                      newSn = updateSnWith sn childSn cost
                  in if edgeCost < h sn && cost < h sn
                     then (newSn, cache')
                     else (sn, cache')

            expandWith sn@(SearchNode pa cv be _ _) v ni
                = SearchNode (pa ++ [v]) Nothing Nothing PosInfinity ni

            updateSnWith sn@(SearchNode pa cv be _ _) 
                         chidlSn@(SearchNode pa' cv' be' _ _ ) cost
                             = sn{currentValue = Just $ last pa', 
                                  bestExtension = append cv' be',
                                  h = cost}
                               
            append (Just a) (Just bs) = Just (a:bs)
            append Nothing (Just bs) = Just bs
            append Nothing Nothing = Nothing
            append (Just a) Nothing = Nothing

dfsWCache sn cache _ [] = (sn { bestExtension = Just [], h = Only 0}, cache)

showCache cache = unlines $ map (\(k, v) -> show (currentValue k) ++ " " ++ show (bestExtension v)) 
                  (HM.toList cache)

showSn (SearchNode pa cv be h ni) = "PA:" ++ show pa ++ "\nCV:" 
                                    ++ showMaybe cv ++ "\n"
                                       where showMaybe Nothing = "Nothing"
                                             showMaybe (Just x) = show x

                    
                


                               

             
    
                           


