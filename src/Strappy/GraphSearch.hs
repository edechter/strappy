-- GraphSearch.hs

module Strappy.GraphSearch where

import Debug.Trace


dfs :: Int -- ^ m (total number of variables)
    -> Int -- ^ n (current variable index)
    -> [a] -- ^ [x_1, .., x_n]
    -> b   -- ^ c_n (computation intermediates up to n)
    -> (Int -> [a] -> b -> (Int, b)) -- ^ w (edge function)
    -> [[a]]
    -> (Int, [a], Int)
dfs m n (x:xs) c w dss | n < (m - 1) = 
    foldl1 minBy dfss
        where dfss = do y <- dss !! (n+1)
                        let (edge, c') = w (n+1) (y:x:xs) c
                            (_, ex, cost) = dfs m (n+1) (y:x:xs) c' w dss
                        return (edge, (y:ex), edge + cost)

              minBy x@(edge, ex, cost) y@(edge', ex', cost') 
                  = if cost < edge' then x else
                        if cost < cost' then x else
                            y
dfs m n (x:xs) c w dss | n == (m - 1) = (0, [], 0)


data SearchSol a = SearchSol { extensionCost :: Int, 
                               extension :: [a]} deriving Show
data SearchNode a = SearchNode { variable :: Int,
                                 assignment :: [a]} deriving (Eq, Show)
                               
type SearchCache a = [(SearchNode a, SearchSol a)] -- ^ a lookup
                                               -- table for dfs'
                                               -- computations.

type GraphEq a = SearchNode a -> SearchNode a -> Bool


dfs' :: Int -- ^ m (total number of variables)
     -> SearchNode a -- ^ (variable number and assignments)
    -> b   -- ^ c_n (computation intermediates up to n)
    -> (Int -> [a] -> b -> (Int, b)) -- ^ w (edge function)
    -> SearchCache a
    -> GraphEq a
    -> [[a]]
    ->
 (SearchSol a, SearchCache a)
dfs' m (SearchNode n xs) c w cache eq dss | n == (m) = (SearchSol 0 [], cache)

dfs' m (SearchNode n xs) c w cache eq dss = 
    let (v:vs) = dss !! (n)
    in foldl minBy (expand v cache) vs
    where
      
      expand y cache 
          = let (edge, c') = w (n+1) (y:xs) c
                childNode =  SearchNode (n+1) (y:xs)
                (childSol, childCache) 
                    = case lookupBy eq childNode cache of
                        Just s -> (s, cache)
                        Nothing -> (s, cache')
                            where (s, cache'') = dfs' m childNode c' w cache eq dss
                                  cache' = insert (childNode, s) cache''
                sol = SearchSol (edge + childCost) (y:childExt)
                      where childCost = extensionCost childSol
                            childExt = extension childSol
            in (sol, childCache)

      minBy x@(SearchSol exCost ex, cache) y 
          = let (edge, c') = w (n+1) (y:xs) c
                x'@(SearchSol exCost' ex', cache') = expand y cache
            in if exCost < edge then x else
                   if exCost < exCost' then x else
                       x'


lookupBy eq node ((n, s):xs) | n `eq` node = Just s
                             | otherwise = lookupBy eq node xs

lookupBy eq node [] = Nothing

insert x cache = (x:cache)




                      

                           
                            
                            


