{-# Language NoMonomorphismRestriction, GeneralizedNewtypeDeriving  #-}

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Error
import System.Random
import Criterion.Main
import Criterion.Config (defaultConfig)
import Strappy.Type
import Strappy.Sample
import Strappy.StdLib (basicGrammar)

newtype Env a = Env {unEnv :: ErrorT String (Rand StdGen) a}
    deriving (Monad,
              MonadRandom,
              MonadPlus,
              MonadError String)

runEnv m g = runRand (runErrorT $ unEnv m ) g

sampleFromBasicGrammar = runStateT (sampleFromGrammar basicGrammar tBool) 0 

-- benchmarks g = [sampleFromBasicGrammar g]

main = newStdGen >>= defaultMain . \g -> [bench "sample" $ whnf (runEnv sampleFromBasicGrammar) g]
    
-- main = defaultMain [bench "sample" $ whnf sampleFromBasicGrammar
-- main = getStdGen >>= sampleFromBasicGrammar
-- main = newStdGen >>= defaultMainWith defaultConfig (return ()) . benchmarks
