
module Strappy.Simplify where

import Strappy.Expr
import Strappy.Library

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
     App { eLeft = App { eLeft = s, eRight = f }, eRight = g } | s == cS ->
       let e' = simplifyExpr ((f <> r') <> (g <> r')) in
       if exprSize e' < exprSize e then e' else e
     -- Simplify arithmetic expressions
     App { eLeft = p, eRight = v1@(Term { eType = tInt }) } | eType r' == tInt && isTerm r' && p == cAdd ->
       cInt2Expr (eThing v1 + eThing r')
     _ -> l' <> r'
simplifyExpr e = e

-- | Simplifies terms in the grammar, subject to the constraint that it reduces description length
--simplifyLibrary :: Grammar -> Grammar
--simplifyLibrary 