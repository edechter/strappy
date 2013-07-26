

module Strappy.Experiments.Words.Grammar where

import Data.List (nub)

import Strappy.Expr
import Strappy.Type
import Strappy.Library
import Strappy.Sample
import Strappy.Planner

import Text.EditDistance

cChar2Expr :: Char -> Expr
-- | Convert char to expr
cChar2Expr c = mkTerm (show c) tChar c

alphabetExprs :: [Expr]
alphabetExprs = map cChar2Expr ['a'..'z']

wordsExprs :: [Expr]
wordsExprs = nub $ basicExprs ++ listExprs ++ alphabetExprs

wordsGr :: Grammar
wordsGr = normalizeGrammar $ Grammar (log 0.45) (mkExprDistr wordsExprs) 

