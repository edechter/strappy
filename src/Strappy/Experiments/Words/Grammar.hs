

module Strappy.Experiments.Words.Grammar where

import Strappy.Expr
import Strappy.Type
import Strappy.Library
import Strappy.Sample

import Data.List (nub)

cChar2Expr :: Char -> Expr
-- | Convert char to expr
cChar2Expr c = mkTerm (show c) tChar c

alphabetExprs :: [Expr]
alphabetExprs = map cChar2Expr ['a'..'z']

wordsExprs :: [Expr]
wordsExprs = nub $ basicExprs ++ alphabetExprs

wordsGr :: Grammar
wordsGr = normalizeGrammar $ Grammar (log 0.45) (mkExprDistr wordsExprs) 