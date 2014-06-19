-- Logging.hs
-- |
-- Module:      Strappy.Core.Logging
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | Useful functions for logging to JSON with Pipes

module Strappy.Logging (
    -- * Logging Functions
    openLog,
    logMessage,
    logGrammar,
    closeLog
    ) where

-- External Imports --
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import           Pipes

-- Strappy imports --
import Strappy.Core.Expr
import Strappy.Core.Grammar

-- | LogValues help provide named JSON objects for logging.
data (LogValue a) = LogValue String a

instance (A.ToJSON a) => A.ToJSON (LogValue a) where
    toJSON (LogValue name value) = A.object [ "type" A..= name,
                                              "value" A..= value ]

instance (A.FromJSON a) => A.FromJSON (LogValue a) where
     parseJSON (A.Object o) = LogValue <$>
                                 o A..: "type" <*>
                                 o A..: "value"
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero

-- | logThis sends some JSON value to the log.
logThis :: (Monad m, A.ToJSON a) => String -> a -> Producer B.ByteString m ()
logThis t v = yield $ B.append (AP.encodePretty $ LogValue t v) (B8.pack ",")

-- | logMessages sends a Message to the log.
logMessage :: (Monad m) => String -> Producer B.ByteString m ()
logMessage s = logThis "message" s

-- | logGrammar sends a Grammar to the log.
logGrammar :: (Monad m) => Grammar -> Producer B.ByteString m ()
logGrammar g = logThis "grammar" g

-- | openLog opens the JSON array we're writing.
openLog :: (Monad m) => Producer B.ByteString m ()
openLog = yield (B8.pack "[")

-- | closeLog closes the JSON array we're writing.
closeLog :: (Monad m) => Producer B.ByteString m ()
closeLog =
    let finalValue = LogValue "message" "Finished!"
    in yield $ B.append (AP.encodePretty finalValue) (B8.pack "\n]")
