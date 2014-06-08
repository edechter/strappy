
import Strappy.EM
import Strappy.Library
import Strappy.Expr 

exprs = [ toUExpr cI,
--         toUExpr cBottom,
         toUExpr cPlus]
--         toUExpr cTimes,
--         toUExpr (intToExpr 1)]
gr = normalizeGrammar $ Grammar (log 0.4) (mkExprDistr exprs)       
 
expr = cPlus <> intToExpr 1 <> intToExpr 1

ll = exprLogLikelihood gr expr
  
dl = descriptionLength gr

main = do
       print ll
