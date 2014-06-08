module Strappy.Case
    where

data Case a b = Case [(a->Bool,b)] b

defaultCase :: b -> Case a b
defaultCase f = Case [] f

addCase :: (Case a b) -> (a->Bool) -> b -> (Case a b)
addCase (Case cs f) g r = Case (cs++[(g,r)]) f

changeDefault  :: (Case a b) -> b -> (Case a b)
changeDefault (Case cs _) f = Case cs f

evalCase :: (Case a b) -> a -> b
evalCase (Case cs f) x = head ((map snd $ (filter (\(guard,_) -> guard x) $ cs)) ++ [f])