-- Parameters.hs
-- |
-- Module:      Strappy.Core.Parameters
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | Dynamic configuration support for Strappy simulations

{-# LANGUAGE TemplateHaskell #-}
module Strappy.Parameters (
    -- * Types
    Parameters(..),
    -- * Conversion
    loadConfig,
    configToParameters,
    -- * access
    writeLog,
    pruneGrammar,
    sampleByEnumeration,
    frontierSize,
    frontierSamples,
    maxEvalTime,
    rndSeed,
    lambda,
    pseudocounts,
    grApp,
    nIters,
    prefix,
    grLibrary,
    tasks
    ) where

-- External imports --
import qualified Control.Lens as L
import           Control.Lens.TH
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import           Data.Text
import           Data.Word

-- Strappy imports --
import Strappy.Core.Expr
import Strappy.Core.Task

-- | Parameters is a product type holding configuration data for Strappy.
data Parameters = Parameters {
    -- (T) log output (grammars, etc), (F) don't log
    _writeLog :: Bool,
    -- (T) prune grammar to search more frontier with less accuracy, (F) don't
    _pruneGrammar :: Bool,
    -- (T) sample by enumeration, (F) sample by true sampling
    _sampleByEnumeration :: Bool,
    -- Max size of the frontier enumerated during the "E" step
    _frontierSize :: Int,
    -- Maximum number of samples drawn from the grammar
    _frontierSamples :: Int,
    -- Timeout for evaluations (in nanoseconds)
    _maxEvalTime :: Word64,
    -- the random seed
    _rndSeed :: Int,
    -- lambda, the threshold productions must pass to be saved
    _lambda :: Double,
    -- pseudocounts for productions in the grammar
    _pseudocounts :: Int,
    -- Probability of an application in the seed grammar
    _grApp :: Double,
    -- The number of iterations to run
    _nIters :: Int,
    -- directory in which to log all output
    _prefix :: FilePath,
    -- the library of primitives in the seed grammar
    _grLibrary :: Library,
    -- all the tasks Strappy should attempt to hit
    _tasks :: TaskSet }

$(makeLenses ''Parameters)

-- | Update the defaults with user-specified values.
loadConfig :: CT.Config -> IO Parameters
loadConfig config = configToParameters config defaultParameters

-- | Update a given parameter set with user-specified values.
configToParameters :: CT.Config -> Parameters -> IO Parameters
configToParameters config params =
    let maybeBool l k p = do
            searchResult <- C.lookup config (pack k)
            return $ maybe p (\(CT.Bool x) -> L.set l x p) searchResult
        maybeString l k p = do
            searchResult <- C.lookup config (pack k)
            return $ maybe p (\(CT.String x) -> L.set l (unpack x) p) searchResult
        maybeInt l k p = do
            searchResult <- C.lookup config (pack k)
            return $ maybe p (\(CT.Number x) -> L.set l (round x) p) searchResult
        -- configurator numbers are integers, so we need to read the rationals.
        maybeNumString l k p = do
            searchResult <- C.lookup config (pack k)
            return $ maybe p (\(CT.String x) -> L.set l (read $ unpack x) p) searchResult
    in return params                                       >>= 
       maybeBool writeLog            "writeLog"            >>=
       maybeBool pruneGrammar        "pruneGrammar"        >>=
       maybeBool sampleByEnumeration "sampleByEnumeration" >>=
       maybeInt frontierSize         "frontierSize"        >>=
       maybeInt frontierSamples      "frontierSamples"     >>=
       maybeInt maxEvalTime          "maxEvalTime"         >>=
       maybeInt rndSeed              "rndSeed"             >>=
       maybeNumString lambda         "lambda"              >>=
       maybeInt pseudocounts         "pseudocounts"        >>=
       maybeString prefix            "prefix"              >>=
       maybeNumString grApp          "grApp"               >>=
       maybeInt nIters               "nIters"

-- | The default Parameters
defaultParameters :: Parameters
defaultParameters = Parameters {
    _writeLog = True,
    _pruneGrammar = False,
    _sampleByEnumeration = True,
    _frontierSize = 1000,
    _frontierSamples = 20000,
    _maxEvalTime = 10000,
    _rndSeed = 0,
    _lambda = 1.0,
    _pseudocounts = 1,
    _grApp = (log 0.375),
    _nIters = 10,
    _prefix = "",
    _grLibrary = [],
    _tasks = [] }
