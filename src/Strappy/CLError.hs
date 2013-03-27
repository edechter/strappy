-- CLError.hs
-- | Defines errors for the CL library. 

module Strappy.CLError where

import Control.Monad.Error
import Text.ParserCombinators.Parsec


data CLError = TypeError String 
             | Parser ParseError
             | Default String

showError :: CLError -> String
showError (TypeError message) = message
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default message) = message

instance Show CLError where show = showError

instance Error CLError  where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either CLError

trapError action = catchError action (return . show) 

extractValue :: ThrowsError a -> a
extractValue (Right val) = val