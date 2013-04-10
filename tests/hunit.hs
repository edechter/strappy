{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main (hunit)
-- Copyright   :  (C) 2012-13 Eyal Dechter
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Eyal Dechter <eyaldechter@gmail.com>
--
-- This module provides a simple hunit test suite for Strappy.
--
-----------------------------------------------------------------------------
module Main where

import Data.Set as Set
import Data.Sequence as Seq

import Strappy.CL
import Strappy.StdLib
import Strappy.Type
import Strappy.Grammar

import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework
import Test.HUnit hiding (test)


----------------------------------------------------------------------
-- combinator Holes tests
case_get_holes1 = 
    let hole =(Hole tBool (Domain [True, False]))
        c = cAnd `app'` cTrue `app'` (CHole hole)
        holesAndPtrs = getHoles $ c
    in Set.fromList holesAndPtrs @?= Set.fromList [(hole, Seq.fromList [R])]

case_get_holes2 = 
    let hole =(Hole tBool (Domain [True, False]))
        c = cAnd `app'` (CHole hole) `app'` (CHole hole)
        holesAndPtrs = getHoles $ c
    in Set.fromList holesAndPtrs @?= Set.fromList [(hole, Seq.fromList [R]), 
                                                   (hole, Seq.fromList [L, R])]

case_resolve_holes = 
    let hole =(Hole tBool (Domain [True, False]))
        c = cAnd `app'` (CHole hole) `app'` (CHole hole)
        cs = Set.fromList $ resolveCombHoles c
    in cs @?= Set.fromList [cAnd `app'` cTrue `app'` cTrue,
                            cAnd `app'` cTrue `app'` cFalse,
                            cAnd `app'` cFalse `app'` cTrue,
                            cAnd `app'` cFalse `app'` cFalse]
----------------------------------------------------------------------

----------------------------------------------------------------------
-- grammar tests -- 
case_count_expansions = 
    let c = cAnd `app'` cTrue `app'` cTrue
    in countExpansions c @?= 2

    

main :: IO ()
main = defaultMain [$testGroupGenerator]