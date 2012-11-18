-- StdLib (Standard Combinator Library)
{-# Language BangPatterns #-}

module StdLib where
import Debug.Trace

import qualified Data.Map as Map
import Type
import CL
import Expr
import qualified CombTrie as CT
 
-- | define common combinators
cI = CNode "I" (Func id) (Map (TVar 0) (TVar 0))
cS = CNode "S" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) (App g x))) typeS 
    where typeS = Map t1 (Map t2 t3)
          t1 = Map a (Map b c)
          t2 = Map a b
          t3 = Map a c
          a = TVar 2
          b = TVar 1
          c = TVar 0
cB = CNode "B" (Func $ \f -> Func $ \g -> Func $ \x -> (App f (App g x))) typeB 
    where typeB = (Map (Map t1 t0) 
                       (Map (Map t2 t1)
                            (Map t2 t0)))
          t2 = TVar 2
          t1= TVar 1
          t0 = TVar 0
cC = CNode "C" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) g )) typeC 
    where typeC = Map t1 (Map t2 t3)
          t1 = Map a (Map b c)
          t2 = b
          t3 = Map a c
          a = TVar 2
          b = TVar 1
          c = TVar 0


cPrim = CNode "PrimRec" prim primType
        where prim = Func $ \c -> Func $ \f -> Func $ \(R i) ->
                      if
                         i <= 0 
                      then c
                      else (App f (App (App (App prim c) f) (R $ i - 1)))
              primType = Map (TVar 0) 
                         (Map (Map (TVar 0) (TVar 0)) (Map Rtype (TVar 0)))

cIfThenElse = CNode "If-Then-Else" (Func $ \(B f) -> Func $ \g -> Func $ \x -> 
                                                 if f then g else x) typeIf
              where typeIf = Map Btype (Map t (Map t t))
                    t = TVar 0

cGT = CNode ">" (Func $ \(R x) ->
                     Func $ \(R y) ->
                         B $ x > y) tp
      where tp = Map Rtype (Map Rtype Btype)

cLT = CNode "<" (Func $ \(R x) ->
                     Func $ \(R y) ->
                         B $ x < y) tp
      where tp = Map Rtype (Map Rtype Btype)

cEQ = CNode "==" (Func $ \(R x) ->
                     Func $ \(R y) ->
                         B $ x == y) tp
      where tp = Map Rtype (Map Rtype Btype)

cAND = CNode "&" (Func $ \(B x) ->
                     Func $ \(B y) ->
                         B $ x && y) tp
      where tp = Map Btype (Map Btype Btype)

cOR = CNode "|" (Func $ \(B x) ->
                     Func $ \(B y) ->
                         B $ x || y) tp
      where tp = Map Btype (Map Btype Btype)

-- cPair = CApp cIfThenElse True

true = CNode "True" (B True) Btype
false = CNode "False" (B False) Btype

stdlib = Map.fromList $ 
         [("I", cI)
         , ("S", cS)
         , ("B", cB)
         , ("C", cC)
        , ("IfThenElse", cIfThenElse)
        , ("True", true)
        , ("False", false)
        , ("|", cOR)
        , ("&", cAND)
        , ("<", cLT)
        , (">", cGT)
        , ("==", cEQ)
         , ("+", dOp2C "+" (+))
--         , ("-", dOp2C "-" (-))
--         , ("*", dOp2C "*" (*))
         , ("1", num2C 1)
         , ("primRec", cPrim)   
           ]

stdMultinomialLib = Map.fromList $ [(c, (1::Int)) | c <- Map.elems stdlib]
stdlibTrie = CT.fromList $ [(c, (1::Int)) | c <- Map.elems stdlib]