-- TestBestFirst.hs

data Or  a = OrLeaf  a | OrNode [And a]
data And a = AndLeaf a | AndNode [Or a]
data AndOr a = AOor (Or a) | AOand (And a)
data Tree a = Leaf a | Node [Tree a] deriving Show

toList :: AndOr a -> [Tree a]
toList (AOor (OrLeaf a))   = [Leaf a]
toList (AOand (AndLeaf a)) = [Leaf a]
toList (AOand (AndNode ors)) = map Node $ powerSet $ map (toList . AOor) ors
toList (AOor (OrNode ands)) = concat $ map (toList . AOand) ands

powerSet (xs:[]) = [[x] | x <- xs]
powerSet (xs:xss) = let yss = powerSet xss
                    in do 
                      x <- xs 
                      ys <- yss
                      return $ x:ys

x = AOor (OrNode [AndLeaf 'a', AndLeaf 'b', 
                  AndNode [ OrNode [AndLeaf 'c', AndLeaf 'd'],
                            OrNode [AndLeaf 'e', AndLeaf 'f']]])



                      
                      



