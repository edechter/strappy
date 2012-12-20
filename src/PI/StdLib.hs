-- StdLib (Standard Combinator Library)
{-# Language BangPatterns #-}

module StdLib where
import Debug.Trace

import qualified Data.Map as Map
import Type
import CL
import Expr
-- import qualified CombTrie as CT

-- | define some oft used type variables
t0 = mkTVar 0 
t1 = mkTVar 1
t2 = mkTVar 2
t3 = mkTVar 3
t4 = mkTVar 4
t5 = mkTVar 5
t6 = mkTVar 6
t7 = mkTVar 7
t8 = mkTVar 8

 
-- | define common combinators
cI = CNode "I" (Func id) ( t0 ->- t0)
cS = CNode "S" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) (App g x))) typeS 
    where typeS = (t2 ->- t1 ->- t0) ->- (t2 ->- t1) ->- (t2 ->- t0)

cB = CNode "B" (Func $ \f -> Func $ \g -> Func $ \x -> (App f (App g x))) typeB 
    where typeB = (t1 ->- t0) ->- (t2 ->- t1) ->- (t2 ->- t0)

cC = CNode "C" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) g )) typeC 
    where typeC = (t2 ->- t1 ->- t0) ->- (t2 ->- t1 ->- t0)


-- cPrim = CNode "PrimRec" prim primType
--         where prim = Func $ \c -> Func $ \f -> Func $ \(R i) ->
--                       if
--                          i <= 0 
--                       then c
--                       else (App f (App (App (App prim c) f) (R $ i - 1)))
--               primType = Map tv0 
--                          (Map (Map tv0 tv0) (Map Rtype tv0 ))

-- cIfThenElse = CNode "If-Then-Else" (Func $ \(B f) -> Func $ \g -> Func $ \x -> 
--                                                  if f then g else x) typeIf
--               where typeIf = Map Btype (Map tv0 (Map tv0 tv0))


-- cGT = CNode ">" (Func $ \(R x) ->
--                      Func $ \(R y) ->
--                          B $ x > y) tp
--       where tp = Map Rtype (Map Rtype Btype)

-- cLT = CNode "<" (Func $ \(R x) ->
--                      Func $ \(R y) ->
--                          B $ x < y) tp
--       where tp = Map Rtype (Map Rtype Btype)

-- cEQ = CNode "==" (Func $ \(R x) ->
--                      Func $ \(R y) ->
--                          B $ x == y) tp
--       where tp = Map Rtype (Map Rtype Btype)

-- cAND = CNode "&" (Func $ \(B x) ->
--                      Func $ \(B y) ->
--                          B $ x && y) tp
--       where tp = Map Btype (Map Btype Btype)

-- cOR = CNode "|" (Func $ \(B x) ->
--                      Func $ \(B y) ->
--                          B $ x || y) tp
--       where tp = Map Btype (Map Btype Btype)

-- cCONS = CNode ":" (Func $ \(R r) -> Func $ \(IntList rs) -> IntList (r:rs)) tp
--         where tp = Map Rtype (Map TyIntList TyIntList)

-- cEmpty = CNode "[]" (IntList []) tp
--          where tp = TyIntList

-- cPair = CApp cIfThenElse True

-- true = CNode "True" (B True) Btype
-- false = CNode "False" (B False) Btype

stdlib = Map.fromList $ 
         [
           ("I", cI)
         , ("S", cS)
         , ("B", cB)
--          , ("C", cC)
--         , ("IfThenElse", cIfThenElse)
--         , ("True", true)
--         , ("False", false)
--         , ("|", cOR)
--         , ("&", cAND)
--         , ("<", cLT)
--         , (">", cGT)
--         , ("==", cEQ)
          , ("+", dOp2C "+" (+))
          , ("-", dOp2C "-" (-))
          , ("*", dOp2C "*" (*))
          , ("1", num2C 1)
--         , ("2", num2C 2)
--         , ("3", num2C 3)
--         , ("4", num2C 4)
--           , ("primRec", cPrim) 
--           , (":", cCONS)
--           , ("[]", cEmpty)
           ]

stdMultinomialLib = Map.fromList $ [(c, (1::Int)) | c <- Map.elems stdlib]
-- stdlibTrie = CT.fromList $ [(c, (1::Int)) | c <- Map.elems stdlib]