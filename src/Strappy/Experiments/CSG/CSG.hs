
module Strappy.Experiments.CSG where

--import Prelude hiding (catch)
--import Control.Exception (catch)
import qualified Data.Foldable as Fold
import Control.Monad

import Strappy.Type
import Strappy.AnAlternativeApproach


tSolid = TCon (TyCon "Solid" Star)
cCircle = CLeaf "circle" (Const "circle") tSolid  

exp = 0.4
cadGrammar = Library exp exprs

samples = replicate 100 $ runTI $ sampleFromGrammar cadGrammar (TAp tList tSolid) 

foo :: [IO (Comb, Int)] -> IO String 
foo (x:xs) = do y <- catch (liftM show x) (\_ -> return " error ")
                y' <- foo xs
                return $ y ++ "\n" ++ y'
foo [] = return ""





