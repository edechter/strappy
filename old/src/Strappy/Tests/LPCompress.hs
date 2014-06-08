
module Strappy.Tests.LPCompress where

import Strappy.ParseExpr
import Strappy.Expr
import Strappy.LPCompress
import Strappy.Library

e = annotateRequested $ parseExpr basicExprs "S * I"
weightedCorpus = [(e, 1.0), (e, 1.0)]
out = compressWeightedCorpus 1.0 1.0 basicGrammar weightedCorpus