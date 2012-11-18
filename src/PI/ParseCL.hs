-- ParseCL.hs

module ParseCL where

-- | standard library imports
import qualified Text.ParserCombinators.Parsec as P
import Text.Parsec.Token 
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec ((<|>), (<?>))
import Text.Parsec.Language (haskellDef)
import Control.Applicative ((<$>), (<*>), (*>), (<*))
import qualified Data.Map as Map
import Control.Monad.Error
import Data.Char

-- | local imports

import CL
import Expr
import CLError
import StdLib

eval :: (Map.Map String Comb) -> String -> ThrowsError Expr
eval lib s =  fmap reduce $ parseExpr lib s >>= comb2Expr 

parseExpr :: (Map.Map String Comb) -> String -> ThrowsError Comb
parseExpr combLib s = case P.parse (expr combLib) "expr" s of
                        Left err -> throwError $ Parser err
                        Right val -> return val

lexer = makeTokenParser haskellDef

expr :: (Map.Map String Comb) -> P.CharParser () Comb
expr lib = P.spaces *> do { x<- singleton lib ; rest x}
       where 
         rest x = P.try (do { f <- spaceOp 
                            ; y <- singleton lib
                            ; rest (f x y "")
                            })
                          <|> return x


spaceOp = do { P.spaces ; return CApp} 
              
open = P.string "(" 
close = P.string ")"
p_expr lib = open *> P.spaces *> expr lib <* P.spaces <* close

node :: (Map.Map String Comb) ->  P.CharParser () Comb
node lib = P.try (do { x <- P.many1 $ P.noneOf "() "
              ; case Map.lookup x lib of
                  (Just cNode) -> return cNode
                  Nothing -> fail  $ " Cannot find combinator " ++ show x})
           <|> P.try (num2C <$> (float lexer))
           <|> P.try (num2C <$> (fromInteger <$> integer lexer))



singleton :: (Map.Map String Comb) ->  P.CharParser () Comb
singleton lib = (node lib) <|> (p_expr lib)
