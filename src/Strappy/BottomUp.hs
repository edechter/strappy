
module Strappy.BottomUp where
--module Main where

import Strappy.Expr
import Strappy.Library
import Strappy.Simplify
import Strappy.ExprTemplate


import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S




data Rewrites = Rewrites [(Expr, [Expr])]

getTemplates :: Expr -> [(Expr, [Expr])]
getTemplates e =
    let arity = getArity e
    in filter (not . null . snd) $ flip map [0..arity] $ \numArgs ->
              let args = map eVar $ map (\x -> -x) [1..numArgs]
                  e' = foldl (<>) e args
                  templates = collectTemplates e'
              in (e', templates)
    where collectTemplates e' =
            case reduceExpr e' of
              Nothing -> []
              Just e'' -> e'' : collectTemplates e''

-- | Append is special, in that it has a conditional branch.
-- getTemplates won't work on it.
appendTemplates :: [(Expr, [Expr])]
appendTemplates = [(cAppend <> cEmpty <> (eVar $ -1),
                    [eVar $ -1]),
                   ((cAppend <> ((cCons <> (eVar $ -1)) <> (eVar $ -2))) <> (eVar $ -3),
                    [((cCons <> (eVar $ -1)) <> ((cAppend <> (eVar $ -2)) <> (eVar $ -3)))])]

{-
main = do
    forM_ [cS, cB, cI, cK, cC, cAppend] $ \comb -> do
        forM_ (getTemplates comb) $ \(lhs, rhs) -> do
            putStrLn $ (show lhs) ++ " -->"
            forM_ rhs $ \r -> putStrLn $ "\t" ++ show r
    putStrLn $ show appendTemplates
-}