-- StdLib (Standard Combinator Library)
{-# Language BangPatterns #-}

module StdLib where
import Debug.Trace

import qualified CombMap as CM
import CombMap (CombMap)
import qualified Data.HashMap as HMap

import Type
import CL
import Expr
import Grammar
import Routers (cS, cB, cC, cSS, cSB, cSC, 
                  cBS, cBB, cBC, cCS, cCB, cCC,
                  cSSS, cBCB, routers)

type NamedLib = HMap.Map String Comb

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
cK = CNode "K" (Func $ \a -> Func $ \b -> a)   ( t0 ->- t1 ->- t0)
-- cS = CNode "S" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) (App g x))) typeS 
--     where typeS = (t2 ->- t1 ->- t0) ->- (t2 ->- t1) ->- (t2 ->- t0)

-- cB = CNode "B" (Func $ \f -> Func $ \g -> Func $ \x -> (App f (App g x))) typeB 
--     where typeB = (t1 ->- t0) ->- (t2 ->- t1) ->- (t2 ->- t0)

-- cC = CNode "C" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) g )) typeC 
--     where typeC = (t1 ->- t2 ->- t0) ->- (t2 ->- t1 ->- t0)

-- cBCS = CNode "BCS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \a 
--                 -> Func $ \b 
--                 -> Func $ \x 
--                 -> ((App (App f b) (App (App g a) x)))) tp 
--     where tp = (t1 ->- t2 ->- t0) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t1 ->- t4 ->- t0

-- cSS = CNode "SS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined

-- cSS = CNode "SR" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined

-- cSS = CNode "SS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined

-- cSS = CNode "SS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined

-- cSS = CNode "SS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined

-- cSS = CNode "SS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined



cPrim = CNode "PrimRec" prim primType
        where prim = Func $ \c -> Func $ \f -> Func $ \(N i) ->
                      if
                         i <= 0 
                      then c
                      else (App f (App (App (App prim c) f) (N $ i - 1)))
              primType = t0 ->- ((t0 ->- t0) ->- tInt ->- t0)



----- Boolean Functions ---------

-- Cond, Not, And, Or, XOr, Any, All

cTrue = CNode "True" (B True) tBool
cFalse = CNode "False" (B False) tBool

cAnd = CNode "&" expr tp
    where expr = Func $ \(B x) -> Func $ \(B y) -> B ( x && y)
          tp = tBool ->- tBool ->- tBool
cOr = CNode "|" expr tp
    where expr = Func $ \(B x) -> Func $ \(B y) -> B ( x || y)
          tp = tBool ->- tBool ->- tBool
cNot = CNode "not" expr tp
    where expr = Func $ \(B x) -> B ( not x)
          tp = tBool ->- tBool
cNand = CNode "nand" expr tp
    where expr = Func $ \(B x) -> Func $ \(B y) -> B ( not (x && y))
          tp = tBool ->- tBool ->- tBool


               
cCond = CNode "Cond" expr tp
    where expr = Func $ \c -> Func $ \t -> Func $ \f
                 -> case c of
                      (B True) -> t
                      (B False) -> f
          tp = tBool ->- t0 ->- t0 ->- t0

----- Maybe Functions ----------

cJust = CNode "Just" (Const "Just") tp
    where tp = t0 ->- (TAp tMaybe t0)
        
cNothing = CNode "Nothing" (Const "Nothing") tp
    where tp = (TAp tMaybe t0)

cFromJust = CNode "FromJust" expr t0
    where expr = Func $ \e -> case e of
                                App (Const "Just") x -> x
                                (Const "Nothing") 
                                     -> ExprError "FromJust applied to Nothing."

----- List Functions --------


cCons = CNode ":" (Const ":") (t0 ->- (TAp tList t0) ->- (TAp tList t0))

cHead = CNode "head" expr tp
    where expr = Func $ \xs -> case xs of
                                 (App (App (Const ":") x) y) -> x
                                 Const "[]" 
                                     -> ExprError "head applied to []"
          tp = (TAp tList t0) ->- t0

cTail = CNode "tail" expr tp
    where expr = Func $ \xs -> case xs of
                                 (App (App (Const ":") x) y) -> y
                                 Const "[]" 
                                     -> ExprError "tail applied to []"
          tp = (TAp tList t0) ->- (TAp tList t0)

cEmpty = CNode "[]" (Const "[]") (TAp tList t0)


cIsEmpty = CNode "isEmpty" expr tp
    where expr = Func $ \xs -> case xs of
                                 Const "[]" -> (B True)
                                 otherwise -> (B False)
          tp = (TAp tList t0) ->- tBool


------ Tuple Functions ----------

cPair = CNode "pair" (Const "pair") (t0 ->- t0 ->- TAp tPair t0)
cFst = CNode "fst" expr tp
       where expr = Func $ \pr -> case pr of
                                    (App (App (Const "pair") x) y) -> x
                                    otherwise -> error "Error in cFst"
             tp = (TAp tPair t0) ->- t0

cSnd = CNode "snd" expr tp
       where expr = Func $ \pr -> case pr of
                                    (App (App (Const "pair") x) y) -> y
                                    otherwise -> error "Error in cFst"
             tp = (TAp tPair t0) ->- t0

cTriple = CNode "triple" (Const "triple") 
           (t0 ->- t0 ->- t0 ->- TAp tTriple t0)

cFst3 = CNode "fst3" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (Const "triple") x) y) z) -> x
                                    otherwise -> error "Error in cFst3"
              tp = (TAp tTriple t0) ->- t0
cSnd3 = CNode "snd3" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (Const "triple") x) y) z) -> y
                                    otherwise -> error "Error in cFst3"
              tp = (TAp tTriple t0) ->- t0
cThrd3 = CNode "thrd3" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (Const "triple") x) y) z) -> y
                                    otherwise -> error "Error in cThrd3"
              tp = (TAp tTriple t0) ->- t0

cQuad = CNode "quad" (Const "quad") 
           (t0 ->- t0 ->- t0 ->- TAp tTriple t0)
cFst4 = CNode "fst4" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (App (Const "quad") x) y) z) w) -> x
                                    otherwise -> error "Error in cFst4"
              tp = (TAp tTriple t0) ->- t0
cSnd4 = CNode "snd4" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (App (Const "quad") x) y) z) w) -> y
                                    otherwise -> error "Error in cSnd4"
              tp = (TAp tTriple t0) ->- t0
cThrd4 = CNode "thrd4" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (App (Const "quad") x) y) z) w) -> z
                                    otherwise -> error "Error in cThrd4"
              tp = (TAp tTriple t0) ->- t0
cFrth4 = CNode "frth4" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (App (Const "quad") x) y) z) w) -> w
                                    otherwise -> error "Error in cFrth4"
              tp = (TAp tTriple t0) ->- t0

stdlib' :: NamedLib
stdlib' = (CM.fromList $ 
         [
           ("I", cI)
--         , ("K", cK)
--         , ("cond", cCond)
        , ("True", cTrue)
        , ("False", cFalse)
--         , ("|", cOr)
--         , ("&", cAnd)
--         , ("not", cNot)
--         , ("<", cLT)
--         , (">", cGT)
--         , ("==", cEQ)
          , ("+", dOp2C "+" (+))
--          , ("-", dOp2C "-" (-))
           , ("*", dOp2C "*" (*))
          ,  ("0", num2C 0)
          ,  ("1", num2C 1)
--          , ("2", num2C 2)
--         , ("3", num2C 3)
--         , ("4", num2C 4)
--           , ("primRec", cPrim) 
--           , (":", cCons)
--           , ("[]", cEmpty)
--            , ("head", cHead)
--            , ("tail", cTail)
--            , ("isEmpty", cIsEmpty)
          , ("pair", cPair)
          , ("fst", cFst)
          , ("snd", cSnd)
          , ("triple", cTriple)
          , ("fst3", cFst3)
          , ("snd3", cSnd3)
          , ("thrd3", cThrd3)
           ]) 
           `CM.union` routers

stdgrammar = Grammar lib c
    where lib  = CM.fromList $ [(c, (-3::Double)) | c <- CM.elems stdlib']
          c = 0 -- (-2)


