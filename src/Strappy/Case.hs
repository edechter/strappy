module Strappy.Case
    where

data Case a b = Case [(a->Bool,a->b)] (a->b)

defaultCase :: (a->b) -> Case a b
defaultCase f = Case [] f 

addCase :: (Case a b) -> (a->Bool,a->b) -> (Case a b)
addCase (Case cases failsafe) newCase = Case (cases++[newCase]) failsafe

changeDefault  :: (Case a b) -> (a->b) -> (Case a b)
changeDefault (Case cases failsafe) newFailSafe = Case cases newFailSafe

evalCase :: (Case a b) -> a -> b
evalCase (Case cases failsafe) x = head ((map snd $ (filter (\(guard,_) -> guard x) $ cases)) ++ [failsafe]) $ x

