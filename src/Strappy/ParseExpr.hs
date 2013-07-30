
module Strappy.ParseExpr where


-- standard library imports
import qualified Text.ParserCombinators.Parsec as P
import Text.Parsec.Token 
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec ((<|>), (<?>))
import Text.Parsec.Language (haskellDef)
import Control.Applicative ( (<*>), (*>), (<*))
import Control.Monad.Trans
import Control.Monad.Identity

import Debug.Trace

-- strappy imports
import Strappy.Expr
import Strappy.Library
import Strappy.Type


fetchTerm :: String -> [Expr] -> Maybe Expr
fetchTerm s (e@Term{eName=r}:xs) | s == r    = Just e
                                 | otherwise = fetchTerm s xs 
fetchTerm s _ = Nothing

parseExpr' :: [Expr] -> String -> Either P.ParseError (TypeInference Identity Expr)
parseExpr' exprs s = P.parse (exprParser exprs) "input combinatory expression" s

parseExpr :: [Expr] -> String -> Expr
parseExpr exprs = fromRight . (parseExpr' exprs)
    where fromRight (Right x) = run x

p = P.try (integer lexer <* P.eof ) <|> p `P.chainl1` (whiteSpace lexer *> return (+)) 

exprParser exprs = factor exprs `P.chainl1` (whiteSpace lexer *> return (<.>)) 

instantiateExprType e = do tp <-instantiateType (eType e)
                           return e{eType=tp}

symb exprs = do symb <- identifier lexer <|> operator lexer <|> fmap show  (integer lexer)
                case fetchTerm symb exprs of 
                     Just a -> return (instantiateExprType a)
                     Nothing -> return $ instantiateExprType (Term symb t Nothing undefined undefined)

term exprs = symb exprs <|> parens lexer (term exprs)

factor exprs = P.try (term exprs) <|> parens lexer (exprParser exprs)

app exprs = do r <- term exprs
               whiteSpace lexer
               l <- exprParser exprs
               return (r <.> l)

               
lexer = makeTokenParser haskellDef

---- Expression simplification

mkDummy str = Term str (TVar 100) undefined undefined undefined
x = mkDummy "x"
y = mkDummy "y"
z = mkDummy "z"
f = mkDummy "f"
g = mkDummy "g"

dummies :: [Expr]
dummies = [x, y, z, f, g]

isDummy :: Expr -> Bool
isDummy e = e `elem` dummies

containsDummy :: Expr -> Bool
containsDummy e@Term{} = isDummy e
containsDummy e@App{eLeft=l, eRight=r} = containsDummy l || containsDummy r

r = return
run = runIdentity . runTI

simplify :: Expr -> Expr
simplify e | (not (containsDummy e)) && (eType e == tInt) = intToExpr (eval e :: Int)

simplify t@Term{} = trace "2" $ t
simplify App{eLeft = App{eLeft=App{eLeft=c, eRight=f}, eRight=g}, eRight=x} | c == cS = simplify $ run $ (r f <.> r x) <.> (r g <.> r x)
                                                                            | c == cC = simplify $ run $ (r f <.> r x) <.> r g
                                                                            | c == cB = simplify $ run $ r f <.> (r g <.> r x)

simplify App{eLeft=c, eRight=x} | c == cI  = simplify x
simplify e@App{eLeft=l, eRight=w} = let x = run $ r (simplify l) <.> (r (simplify w))
                                    in if x == e then x else simplify x




