-- StdLib (Standard Combinator Library)
{-# Language BangPatterns #-}

module StdLib where
import Debug.Trace

import qualified Data.Map as Map
import Type
import CL
import Expr
import qualified CombTrie as CT

-- | define some oft used type variables
tv0 = mkTVar 0
tv1 = mkTVar 1
tv2 = mkTVar 2
tv3 = mkTVar 3

 
-- | define common combinators
cI = CNode "I" (Func id) (Map tv0 tv0)
cS = CNode "S" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) (App g x))) typeS 
    where typeS = Map t1 (Map t2 t3)
          t1 = Map tv2 (Map tv1 tv0)
          t2 = Map tv2 tv1
          t3 = Map tv2 tv0

cB = CNode "B" (Func $ \f -> Func $ \g -> Func $ \x -> (App f (App g x))) typeB 
    where typeB = (Map (Map tv1 tv0) 
                       (Map (Map tv2 tv1)
                            (Map tv2 tv0)))

cC = CNode "C" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) g )) typeC 
    where typeC = Map t1 (Map t2 t3)
          t1 = Map tv2 (Map tv1 tv0)
          t2 = tv1
          t3 = Map tv2 tv0

cPrim = CNode "PrimRec" prim primType
        where prim = Func $ \c -> Func $ \f -> Func $ \(R i) ->
                      if
                         i <= 0 
                      then c
                      else (App f (App (App (App prim c) f) (R $ i - 1)))
              primType = Map tv0 
                         (Map (Map tv0 tv0) (Map Rtype tv0 ))

cIfThenElse = CNode "If-Then-Else" (Func $ \(B f) -> Func $ \g -> Func $ \x -> 
                                                 if f then g else x) typeIf
              where typeIf = Map Btype (Map tv0 (Map tv0 tv0))


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

cCONS = CNode ":" (Func $ \(R r) -> Func $ \(IntList rs) -> IntList (r:rs)) tp
        where tp = Map Rtype (Map TyIntList TyIntList)

cEmpty = CNode "[]" (IntList []) tp
         where tp = TyIntList

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
         , ("-", dOp2C "-" (-))
         , ("*", dOp2C "*" (*))
         , ("1", num2C 1)
         , ("primRec", cPrim) 
         , (":", cCONS)
         , ("[]", cEmpty)
           ]

stdMultinomialLib = Map.fromList $ [(c, (1::Int)) | c <- Map.elems stdlib]
stdlibTrie = CT.fromList $ [(c, (1::Int)) | c <- Map.elems stdlib]