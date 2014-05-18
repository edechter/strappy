-- StdLib (Standard Combinator Library)
{-# Language BangPatterns #-}

module Strappy.StdLib where
import Debug.Trace

import qualified Data.HashMap as HMap

import qualified Strappy.CombMap as CM
import Strappy.CombMap (CombMap)
import Strappy.Type
import Strappy.CL
import Strappy.Expr
import Strappy.Grammar
import Strappy.Routers (cS, cB, cC, cSS, cSB, cSC, 
                  cBS, cBB, cBC, cCS, cCB, cCC,
                  cSSS, cBCB, one_routers, routers)

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
cI = CLeaf "I" (Func id) ( t0 ->- t0)
cK = CLeaf "K" (Func $ \a -> Func $ \b -> a)   ( t0 ->- t1 ->- t0)
-- cS = CLeaf "S" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) (App g x))) typeS 
--     where typeS = (t2 ->- t1 ->- t0) ->- (t2 ->- t1) ->- (t2 ->- t0)

-- cB = CLeaf "B" (Func $ \f -> Func $ \g -> Func $ \x -> (App f (App g x))) typeB 
--     where typeB = (t1 ->- t0) ->- (t2 ->- t1) ->- (t2 ->- t0)

-- cC = CLeaf "C" (Func $ \f -> Func $ \g -> Func $ \x -> (App (App f x) g )) typeC 
--     where typeC = (t1 ->- t2 ->- t0) ->- (t2 ->- t1 ->- t0)

-- cBCS = CLeaf "BCS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \a 
--                 -> Func $ \b 
--                 -> Func $ \x 
--                 -> ((App (App f b) (App (App g a) x)))) tp 
--     where tp = (t1 ->- t2 ->- t0) ->- (t3 ->- t4 ->- t2) ->- t3 ->- t1 ->- t4 ->- t0

-- cSS = CLeaf "SS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined

-- cSS = CLeaf "SR" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined

-- cSS = CLeaf "SS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined

-- cSS = CLeaf "SS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined

-- cSS = CLeaf "SS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined

-- cSS = CLeaf "SS" (Func $ \f 
--                 -> Func $ \g 
--                 -> Func $ \x1 
--                 -> Func $ \x2
--                 -> ((App (App x1 x2) (App x1 x2 )) tp 
--     where tp = undefined



cPrim = CLeaf "PrimRec" prim primType
        where prim = Func $ \c -> Func $ \f -> Func $ \(N i) ->
                      if
                         i <= 0 
                      then c
                      else (App f (App (App (App prim c) f) (N $ i - 1)))
              primType = t0 ->- ((t0 ->- t0) ->- tInt ->- t0)



----- Boolean Functions ---------

-- Cond, Not, And, Or, XOr, Any, All

cTrue = CLeaf "True" (B True) tBool
cFalse = CLeaf "False" (B False) tBool

cAnd = CLeaf "&" expr tp
    where expr = Func $ \(B x) -> Func $ \(B y) -> B ( x && y)
          tp = tBool ->- tBool ->- tBool
cOr = CLeaf "|" expr tp
    where expr = Func $ \(B x) -> Func $ \(B y) -> B ( x || y)
          tp = tBool ->- tBool ->- tBool
cNot = CLeaf "not" expr tp
    where expr = Func $ \(B x) -> B ( not x)
          tp = tBool ->- tBool
cNand = CLeaf "nand" expr tp
    where expr = Func $ \(B x) -> Func $ \(B y) -> B ( not (x && y))
          tp = tBool ->- tBool ->- tBool


               
cCond = CLeaf "Cond" expr tp
    where expr = Func $ \c -> Func $ \t -> Func $ \f
                 -> case c of
                      (B True) -> t
                      (B False) -> f
          tp = tBool ->- t0 ->- t0 ->- t0

----- Maybe Functions ----------

cJust = CLeaf "Just" (Const "Just") tp
    where tp = t0 ->- (TAp tMaybe t0)
        
cNothing = CLeaf "Nothing" (Const "Nothing") tp
    where tp = (TAp tMaybe t0)

cFromJust = CLeaf "FromJust" expr (TAp tMaybe t0 ->- t0)
    where expr = Func $ \e -> case e of
                                App (Const "Just") x -> x
                                (Const "Nothing") 
                                     -> ExprError "FromJust applied to Nothing."

----- List Functions --------


cCons = CLeaf ":" (Const ":") (t0 ->- (TAp tList t0) ->- (TAp tList t0))

cHead = CLeaf "head" expr tp
    where expr = Func $ \xs -> case xs of
                                 (App (App (Const ":") x) y) -> x
                                 Const "[]" 
                                     -> ExprError "head applied to []"
          tp = (TAp tList t0) ->- t0

cTail = CLeaf "tail" expr tp
    where expr = Func $ \xs -> case xs of
                                 (App (App (Const ":") x) y) -> y
                                 Const "[]" 
                                     -> ExprError "tail applied to []"
          tp = (TAp tList t0) ->- (TAp tList t0)

cEmpty = CLeaf "[]" (Const "[]") (TAp tList t0)


cIsEmpty = CLeaf "isEmpty" expr tp
    where expr = Func $ \xs -> case xs of
                                 Const "[]" -> (B True)
                                 otherwise -> (B False)
          tp = (TAp tList t0) ->- tBool


------ Tuple Functions ----------

cPair = CLeaf "pair" (Const "pair") (t0 ->- t0 ->- TAp tPair t0)
cFst = CLeaf "fst" expr tp
       where expr = Func $ \pr -> case pr of
                                    (App (App (Const "pair") x) y) -> x
                                    otherwise -> error "Error in cFst"
             tp = (TAp tPair t0) ->- t0

cSnd = CLeaf "snd" expr tp
       where expr = Func $ \pr -> case pr of
                                    (App (App (Const "pair") x) y) -> y
                                    otherwise -> error "Error in cFst"
             tp = (TAp tPair t0) ->- t0

cTriple = CLeaf "triple" (Const "triple") 
           (t0 ->- t0 ->- t0 ->- TAp tTriple t0)

cFst3 = CLeaf "fst3" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (Const "triple") x) y) z) -> x
                                    otherwise -> error "Error in cFst3"
              tp = (TAp tTriple t0) ->- t0
cSnd3 = CLeaf "snd3" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (Const "triple") x) y) z) -> y
                                    otherwise -> error "Error in cFst3"
              tp = (TAp tTriple t0) ->- t0
cThrd3 = CLeaf "thrd3" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (Const "triple") x) y) z) -> y
                                    otherwise -> error "Error in cThrd3"
              tp = (TAp tTriple t0) ->- t0

cQuad = CLeaf "quad" (Const "quad") 
           (t0 ->- t0 ->- t0 ->- TAp tTriple t0)
cFst4 = CLeaf "fst4" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (App (Const "quad") x) y) z) w) -> x
                                    otherwise -> error "Error in cFst4"
              tp = (TAp tTriple t0) ->- t0
cSnd4 = CLeaf "snd4" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (App (Const "quad") x) y) z) w) -> y
                                    otherwise -> error "Error in cSnd4"
              tp = (TAp tTriple t0) ->- t0
cThrd4 = CLeaf "thrd4" expr tp
        where expr = Func $ \tr -> case tr of
                                    (App (App (App (App (Const "quad") x) y) z) w) -> z
                                    otherwise -> error "Error in cThrd4"
              tp = (TAp tTriple t0) ->- t0
cFrth4 = CLeaf "frth4" expr tp
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
        -- , ("True", cTrue)
        -- , ("False", cFalse)
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
          -- , ("pair", cPair)
          -- , ("fst", cFst)
          -- , ("snd", cSnd)
          -- , ("triple", cTriple)
          -- , ("fst3", cFst3)
          -- , ("snd3", cSnd3)
          -- , ("thrd3", cThrd3)
           ]) 
           `CM.union` one_routers

stdgrammar = Grammar lib c
    where lib  = CM.fromList $ [(c, (-3::Double)) | c <- CM.elems stdlib']
          c = 0 -- (-2)

basicGrammar = Grammar lib c
    where lib = CM.fromList $ [(c, 0) | c <- [cI, cTrue, cFalse, cAnd, cOr]]
          c = 0.5


