-- Simulation.hs
-- |
-- Module:      Strappy.Core.Simulation
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | A wrapper function for Strappy learning simulations
module Strappy.Simulation (runSimulation) where

-- External imports --
import qualified Control.Lens as L
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.Configurator as C
import qualified Data.Map as M
import           Pipes
import           System.Random

-- Strappy imports --
import Strappy.Expr
import Strappy.Grammar
import Strappy.Library
import Strappy.Logging
import Strappy.Parameters
import Strappy.Task

-- | A wrapper to run simulations with a single function call.
runSimulation :: FilePath                -- ^ Config Filename
              -> (Parameters -> TaskSet) -- ^ creates tasks given configuration
              -> Library                 -- ^ seed grammar primitives
              -> (B.ByteString -> IO ())            -- ^ Logging Function
              -> IO ()
runSimulation c t l logFn = runEffect $ for (simulation c t l) (lift . logFn)

-- | Build an initial distribution for the seed grammar
initExprDistr :: Library -> ExprDistr
initExprDistr lib = M.fromList $ Prelude.zip lib [1,1..]

-- | Rearrange the foldM arguments to act more like a loop.
loopM :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
loopM start xs step = foldM step start xs

-- | A way to run simple learning simulations in Strappy 
simulation :: FilePath                -- ^ configuration file name
           -> (Parameters -> TaskSet) -- ^ creates tasks given configuration
           -> Library                 -- ^ seed grammar primitives
           -> Producer B.ByteString IO ()
simulation c t l = do
    openLog
    config <- lift $ C.load [C.Required c]
    params <- lift $ loadConfig config
    let tasks' = t params
    let params'= L.set grLibrary l (L.set tasks tasks' params)
    lift $ setStdGen $ mkStdGen (L.view rndSeed params)
    let seedGrammar = normalizeGrammar $
         Grammar {Strappy.Grammar.grApp = 
                      (L.view Strappy.Parameters.grApp params'),
                  grExprDistr = initExprDistr 
                      (L.view grLibrary params') }
    logMessage "Test 0"
    grammar'' <- loopM seedGrammar [1..(L.view nIters params')] $ 
        \grammar step -> do
            logMessage $ "Test " ++ (show step)
            return grammar
--          grammar' <- normalizeGrammar $ 
--                          doEMIter ((get prefix params) ++ (show step))
--                                    (get tasks params)
--                                    (get lambda params)
--                                    (get pseudocounts params)
--                                    (get frontierSize params)
--                                    seedGrammar
--                                    grammar
--          return grammar'
    logGrammar grammar''
    closeLog
    return ()
