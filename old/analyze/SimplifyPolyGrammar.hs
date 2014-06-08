
module SimplifyPolyGrammar where

import Control.Arrow (first, second)
import Data.List
import Data.Function (on)
import Text.Printf

import Strappy.ParseExpr
import Strappy.Library
import Strappy.Expr

split :: String -> (String, String)
split (x:xs) | not (x == ' ') = (x : y , z) where (y, z) = split xs
split (x:xs)   = ([], xs)
split [] = ([], [])

file = "data/f10000poly_grammar_5"



simplifyAndPrint :: String -> String
simplifyAndPrint xs = let ls = take 10 $ lines xs
                          p = take 1 ls
                          gr = drop 1 ls
                          onEach :: String -> (Double, Expr)
                          onEach = first (read :: String -> Double)
                                  . second (simplifyWithDummies . parseExpr basicExprs ) . split
                          dt = map onEach gr
                          dt' = reverse $ sortBy (compare `on` fst) dt
                          combine (a, b) = printf "%40s ---------- %.2f\n" (show b) a
                      in unlines $ (p ++ map combine dt')

main = do xs <- readFile file
          putStrLn $ simplifyAndPrint xs
