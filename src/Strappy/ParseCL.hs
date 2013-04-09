-- ParseCL.hs
-- | A module for parsing strings into combinators. 
module Strappy.ParseCL (
                        eval
                       , parseExpr
                       , parseExprStd
                       ) where

-- standard library imports
import qualified Text.ParserCombinators.Parsec as P
import Text.Parsec.Token 
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec ((<|>), (<?>))
import Text.Parsec.Language (haskellDef)
import Control.Applicative ((<$>), (<*>), (*>), (<*))
import qualified Data.HashMap as HMap
import Control.Monad.Error
import Control.Monad.State
import Data.Char

-- local imports
import Strappy.Type
import Strappy.CL
import Strappy.Expr
import Strappy.StdLib 
import Strappy.Compress
import qualified Strappy.CombMap as CM
import Strappy.CombMap (CombMap)


eval :: NamedLib -- ^ combinator library
        -> String -- ^  input string
        -> Either String Expr 
-- | Evaluate string to expression. 
eval lib s =  liftM reduceComb $ parseExpr lib s

parseExpr :: NamedLib -- ^ combinator library
          -> String -- ^ input string
          -> SynthComb -- ^ output in Either String Comb
-- | Parse combinator to string.
parseExpr combLib s = case P.parse (expr combLib) "expr" s of
                        Left err -> Left (show err)
                        Right val -> val

-- | Parse combinator using stdlib 
parseExprStd = parseExpr stdlib'

lexer = makeTokenParser haskellDef

expr :: NamedLib -> P.CharParser () SynthComb
expr lib = P.spaces *> do { x<- singleton lib ; rest x}
       where 
         rest x = do { f <- spaceOp 
                            ; y <- singleton lib
                            ; rest $ x <:> y 
                            }
                     <|> return x


spaceOp = do { P.spaces ; return CApp} 
              
open = P.string "(" 
close = P.string ")"
p_expr lib = open *> P.spaces *> expr lib <* P.spaces <* close

node :: NamedLib ->  P.CharParser () SynthComb
node lib = P.try (do { x <- P.many1 $ P.noneOf "() "
              ; case HMap.lookup x lib of
                  (Just cNode) -> return (Right cNode)
                  Nothing -> fail  $ " Cannot find combinator " ++ show x})
           <|> P.try (return <$> num2C <$> (fromInteger <$> integer lexer))

singleton :: NamedLib ->  P.CharParser () SynthComb
singleton lib = (node lib) <|> (p_expr lib)


 

