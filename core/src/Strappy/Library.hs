-- Library.hs
-- |
-- Module:      Strappy.Core.Library
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | This module defines collections of standard primitives and types
-- for our expression language.

module Strappy.Library where

-- External imports --
import Control.Monad.Error.Class
import Data.Maybe
import qualified Data.List as List

-- Strappy imports -- 
import Strappy.Type
import Strappy.Expr
import Strappy.Grammar

-- | COMBINATORS/PRIMITIVE EXPRESSIONS | ---------------------------------------
-- In this file, we list a few primitives likely to be useful in many libraries.

cBasicRouters = [cI, cS, cB, cC, cK]

cI = Term { eName = "I",
            eType = (t ->- t),
            eThing = id }

cS = Term { eName  = "S",
            eType  = ((t2 ->- t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t),
            eThing = (\f g x -> f x (g x)) }

cB = Term { eName  = "B",
            eType  = ((t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t),
            eThing = (\f g x -> f (g x)) }

cC = Term { eName  = "C",
            eType  = ((t1 ->- t2 ->- t) ->- t2 ->- t1 ->- t),
            eThing = (\f g x -> f x g) }

cK = Term { eName  = "K",
            eType  = (t1 ->- t2 ->- t1),
            eThing = (\x y -> x) }

cDualRouters = [cSS,cSB,cSC,cBS,cBB,cBC,cCS,cCB,cCC]

-- | cSS = (cB <> cS <> (cB <> cS))
cSS = Term { eName  = "SS",
             eType  = ((t2 ->- t3 ->- t1 ->- t) ->-
                      (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t),
             eThing = (\f g x1 x2 -> (f x1 x2) (g x1 x2)) }

-- | cSB = (cB <> cS <> (cB <> cB))
cSB = Term { eName  = "SB",
             eType  = ((t2 ->- t1 ->- t) ->-
                      (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t),
             eThing = (\f g x1 x2 -> (f x1) (g x1 x2)) }

-- | cSC = (cB <> cS <> (cB <> cC))
cSC = Term { eName  = "SC",
             eType  = ((t3 ->- t1 ->- t2 ->- t) ->-
                       (t3 ->- t2) ->- t3 ->- t1 ->- t),
             eThing = (\f g x1 x2 -> (f x1 x2) (g x1)) }

-- | cBS = (cB <> cB <> cS)
cBS = Term { eName  = "BS",
             eType  = ((t3 ->- t1 ->- t) ->-
                      (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t),
             eThing = (\f g x1 x2 -> (f x2) (g x1 x2)) }

-- | cBB = (cB <> cB <> cB)
cBB = Term { eName  = "BB",
             eType  = ((t1 ->- t) ->- (t2 ->- t3 ->- t1) ->- t2 ->- t3 ->- t),
             eThing = (\f g x1 x2 -> f (g x1 x2)) }

-- | cBC = (cB <> cB <> cC)
cBC = Term { eName  = "BC",
             eType  = ((t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t3 ->- t1 ->- t),
             eThing = (\f g x1 x2 -> (f x2) (g x1)) }

-- | cCS = (cB <> cC <> (cB <> cS))
cCS = Term { eName  = "CS",
             eType  = ((t1 ->- t3 ->- t2 ->- t) ->-
                      (t3 ->- t2) ->- t1 ->- t3 ->- t),
             eThing = (\f g x1 x2 -> (f x1 x2) (g x2)) }

-- | cCB = (cB <> cC <> (cB <> cB))
cCB = Term { eName  = "CB",
             eType  = ((t1 ->- t2 ->- t) ->- (t3 ->- t2) ->- t1 ->- t3 ->- t),
             eThing = (\f g x1 x2 -> (f x1) (g x2)) }

-- | cCC = (cB <> cC <> (cB <> cC))
cCC = Term { eName  = "CC",
             eType  = ((t1 ->- t2 ->- t3 ->- t) ->- t3 ->- t1 ->- t2 ->- t),
             eThing = (\f g x1 x2 -> (f x1 x2) (g)) }

-- | Bottom
cBottom = Term { eName  = "_|_",
                 eType  = (TVar 0),
                 eThing = (error "cBottom: this should never be called!") }

-- | Holes
cHole = Term { eName  = "H",
               eType  = tDouble,
               eThing = (error "Attempt to evaluate a hole") }


cPairs = [cPair, cFst, cSnd, cOnFst, cOnSnd, cSwap]

cPair = Term { eName  = "pair",
               eType  = (t1 ->- t2 ->- tPair t1 t2),
               eThing = (,) }

cFst = Term { eName  = "fst",
              eType  = (tPair t1 t2 ->- t1),
              eThing = fst }

cSnd = Term { eName  = "snd",
              eType  = (tPair t1 t2 ->- t2),
              eThing = snd }

cOnFst = Term { eName  = "onFst",
                eType  = ((t1 ->- t2) ->- (tPair t1 t) ->- (tPair t2 t)),
                eThing = (\f (a,b) -> (f a, b)) }

cOnSnd = Term { eName  = "onSnd",
                eType  = ((t1 ->- t2) ->- (tPair t t1) ->- (tPair t t2)),
                eThing = (\f (a,b) -> (a, f b)) }

cSwap = Term { eName  = "swap",
               eType  = ((tPair t1 t2) ->- (tPair t2 t1)),
               eThing = (\(a,b) -> (b,a)) }

cTriplets = [cTriplet, cFst3, cSnd3, cTrd3]

cTriplet = Term { eName  = "triplet",
                  eType  = (t ->- t1 ->- t2 ->- tTriplet t t1 t2),
                  eThing = (\x y z -> (x,y,z)) }

cFst3 = Term { eName  = "fst3",
               eType  = (tTriplet t t1 t2 ->- t ),
               eThing = (\(x,_,_) -> x) }

cSnd3 = Term { eName  = "snd3",
               eType  = (tTriplet t t1 t2 ->- t1),
               eThing = (\(_,x,_) -> x) }

cTrd3 = Term { eName  = "trd3",
               eType  = (tTriplet t t1 t2 ->- t2),
               eThing = (\(_,_,x) -> x) }

cIntOps = [cPlus, cTimes, cMinus]

cPlus = Term { eName  = "+",
               eType  = (tInt ->- tInt ->- tInt),
               eThing = (+) }

cTimes = Term { eName  = "*",
                eType  = (tInt ->- tInt ->- tInt),
                eThing = (*) }

cMinus = Term { eName  = "-",
                eType  = (tInt ->- tInt ->- tInt),
                eThing = (-) }

cDoubleOps = [cFPlus, cFDiv, cFTimes, cFMinus] 

cFPlus = Term { eName  = "+.",
                eType  = (tDouble ->- tDouble ->- tDouble),
                eThing = ((+) :: Double -> Double -> Double) }

cFDiv = Term { eName  = "/.",
               eType  = (tDouble ->- tDouble ->- tDouble),
               eThing = ((/) :: Double -> Double -> Double) }

cFTimes = Term { eName  = "*.",
                 eType  = (tDouble ->- tDouble ->- tDouble),
                 eThing = ((*) :: Double -> Double -> Double) }

cFMinus = Term { eName  = "-.",
                 eType  = (tDouble ->- tDouble ->- tDouble),
                 eThing = ((-) :: Double -> Double -> Double) }

cListOps = [cCons, cAppend, cHead, cTail, cMap, cFilter, cEmpty, cSingle, cRep,
            cConcat, cReverse, cFoldl, cFoldr, cFoldl1, cFoldr1]

cCons = Term { eName  = ":",
               eType  = (t ->- tList t ->- tList t),
               eThing = (:) }

cAppend = Term { eName  = "++",
                 eType  = (tList t ->- tList t ->- tList t),
                 eThing = (++) }

cHead = Term { eName  = "head",
               eType  = (tList t ->- t),
               eThing = head }

cTail = Term { eName  = "tail",
               eType  = (tList t ->- tList t),
               eThing = tail }

cMap = Term { eName  = "map",
              eType  = ((t ->- t1) ->- tList t ->- tList t1),
              eThing = map }

cFilter = Term { eName  = "filter",
                 eType  = ((t ->- tBool) ->- tList t ->- tList t),
                 eThing = filter }

cEmpty = Term { eName  = "[]",
                eType  = (tList t),
                eThing = [] }

cSingle = Term { eName  = "single",
                 eType  = (t ->- tList t),
                 eThing = (\x -> [x]) }

cRep = Term { eName  = "rep",
              eType  = (tInt ->- t ->- tList t),
              eThing = replicate }

cConcat = Term { eName  = "concat",
                 eType  = (tList (tList t) ->- tList t),
                 eThing = concat }

cReverse = Term { eName  = "reverse",
                  eType  = (tList t ->- tList t),
                  eThing = reverse }

cFoldl = Term { eName  = "foldl",
                eType  = ((t ->- t1 ->- t) ->- t ->- tList t1 ->- t),
                eThing = List.foldl' }

cFoldr = Term { eName  = "foldr",
                eType  = ((t1 ->- t2 ->- t2) ->- t2 ->- tList t1 ->- t2),
                eThing = List.foldr }

cFoldl1 = Term { eName  = "foldl1",
                 eType  = ((t ->- t ->- t) ->- tList t ->- t),
                 eThing = foldl1 }

cFoldr1 = Term { eName  = "foldr1",
                 eType  = ((t ->- t ->- t) ->- tList t ->- t),
                 eThing = foldr1 }

cBoolOps = [cNand, cAnd, cOr, cNot, cIf]

cNand = Term { eName  = "nand",
               eType  = (tBool ->- tBool ->- tBool),
               eThing = (\x y -> not (x && y)) }

cAnd  = Term { eName  = "and",
               eType  = (tBool ->- tBool ->- tBool),
               eThing = (&&) }

cOr   = Term { eName  = "or",
               eType  = (tBool ->- tBool ->- tBool),
               eThing = (||) }

cNot  = Term { eName  = "not",
               eType  = (tBool ->- tBool),
               eThing = not }

cIf = Term { eName  = "If",
             eType  = (tBool ->- t ->- t ->- t),
             eThing = (\ p x y -> if p then x else y) }

cMaybes = [cJust, cNothing, cFromJust, cMaybe]

cJust = Term { eName  = "Just",
               eType  = (t ->- tMaybe t),
               eThing = Just }

cNothing = Term { eName  = "Nothing",
                  eType  = (tMaybe t),
                  eThing = Nothing } 

cFromJust = Term { eName  = "fromJust",
                   eType  = (tMaybe t ->- t),
                   eThing = (\ x -> safeFromJust "(cFromJust cNothing)" x) }

cMaybe = Term { eName = "maybe",
                eType = (t2 ->- (t1 ->- t2) ->- tMaybe t1 ->- t2),
                eThing = maybe }

cInts    = [intToExpr i | i <- [-10..10]]

cDoubles = [doubleToExpr d | d <- [-10..10]]

cChars   = [charToExpr c | c <- ['a'..'z']]

cBools   = [(boolToExpr True), (boolToExpr False)]

basicExprs = cBasicRouters ++
             cDualRouters  ++
             cIntOps       ++
             cDoubleOps    ++
             cListOps      ++
             cBoolOps      ++
             cPairs        ++
             cTriplets     ++
             cInts         ++
             cDoubles      ++
             cChars        ++
             cBools        ++
             [cHole]
