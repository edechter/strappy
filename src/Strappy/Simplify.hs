
--module Strappy.Simplify where
module Main where

import Strappy.Expr
import Strappy.Library

import Data.Function
import Data.List
import Unsafe.Coerce (unsafeCoerce) 
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

simplifyExpr :: Expr -> Expr
simplifyExpr e@(App { eLeft = l, eRight = r }) =
  let l' = simplifyExpr l
      r' = simplifyExpr r
  in
   case l' of
     Term { } | l' == cI -> r'
     App { eLeft = k, eRight = v } | k == cK -> v
     App { eLeft = App { eLeft = b, eRight = f }, eRight = g } | b == cB -> simplifyExpr (f <> (g <> r'))
     App { eLeft = App { eLeft = c, eRight = f }, eRight = g } | c == cC -> simplifyExpr ((f <> r') <> g)
     -- For S, we may or may not reduce the size of the expression
     -- TODO FIXME: We also need to check that the branches don't have holes in them.
     App { eLeft = App { eLeft = s, eRight = f }, eRight = g } | s == cS ->
       let e' = simplifyExpr ((f <> r') <> (g <> r')) in
       if exprSize e' < exprSize e then e' else e
     -- Simplify arithmetic expressions
{-     App { eLeft = p, eRight = v1@(Term { eType = tInt, eThing = t1 }) } | eType r' == tInt && isTerm r' && p == cPlus ->
       case r' of
         Term { eThing = t2 } -> cInt2Expr (unsafeCoerce t1 + unsafeCoerce t2)
         _ -> error "Right has isTerm true, but is not terminal"-}
     _ -> l' <> r'
simplifyExpr e = e
              
main = do
  lib <- loadGrammar "poly_grammar_20"
  let prods = Set.toList $ foldl (\acc prod -> collectSubtrees acc prod) Set.empty $ filter (not . isTerm) $ Map.keys $ grExprDistr lib
  putStrLn $ show prods
  putStrLn $ show $ length prods
  let prods' = map annotateRequested prods
  let prods'' = simplifyLibrary prods'
  putStrLn $ show $ prods''
  putStrLn $ show $ length prods''