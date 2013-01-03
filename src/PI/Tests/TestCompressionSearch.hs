-- TestCompressionSearch.hs
{-# Language TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses#-}

module TestCompressionSearch where

import Test.QuickCheck
import Data.List 
import Control.Monad
import Control.Monad.State

import CompressionSearch
import ParseCL
import StdLib (stdlib, stdlib')
import CL
import Type
import qualified CombMap as CM
import CombMap (CombMap)
import Task
import GraphSearch2
import Infinite

fromRight (Right x) = x

-- test greedy search
r = map (fromRight . parseExpr stdlib') 
t1 = mkSingleEqualityTask 100 1
t2 = mkSingleEqualityTask 100 4
t3 = mkSingleEqualityTask 100 9
t4 = mkSingleEqualityTask 100 16
t5 = mkSingleEqualityTask 100 25
y1 = r [ "S * I 1", "(* 1 1)"]
y2 = r ["(* 2 2)", "S * I 2"]
y3 = r ["(* 3 3)", "S * I 3"]
y4 = r ["(* 4 4)", "S * I 4"]
y5 = r ["(* 5 5)", "S * I 5"]
y6 = r ["(* 6 6)", "S * I 6"]
y7 = r ["(* 7 7)", "S * I 7"]
y8 = r ["(* 8 8)", "S * I 8"]
y9 = r ["(* 9 9)", "S * I 9"]

xs = [y1,  y2, y3, y4,  y5, y6, y7, y8, y9]

                  


-- updateState :: SearchNode Comb (CombMap Int) -> Comb -> (CombMap Int, Infinite Int)
-- updateState sn v = (ind', n' - n)
--     where 
--       ind = nodeInfo sn
--       ind' = updateIndex (nodeInfo sn) v
--       f = (Only . length . CM.keys)
--       n = f ind
--       n'= f ind'

w n (x:xs) c = (n' - n, ind')
    where ind = c
          ind' = updateIndex ind x
          f = (length . CM.keys)
          n = f ind
          n' = f ind'

eq node1 node2 = (last $ assignment $ node1) == (last $ assignment $ node2)
eq' node1 node2 = node1 == node2

out = dfs' (length xs) (SearchNode 0 []) CM.empty w [] eq xs


                     




     









