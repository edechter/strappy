{-# Language NoMonomorphismRestriction, GeneralizedNewtypeDeriving  #-}

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Error
import Control.DeepSeq
import System.Random
import Criterion.Main
import Criterion.Config (defaultConfig)

import Strappy.Expr
import Strappy.CL
import Strappy.Type
import Strappy.Sample
import Strappy.Grammar
import Strappy.StdLib (basicGrammar)

newtype Env a = Env {unEnv :: ErrorT String (Rand StdGen) a}
    deriving (Monad,
              MonadRandom,
              MonadPlus,
              MonadError String)

runEnv m g = runRand (runErrorT $ unEnv m ) g

sampleFromBasicGrammar p = runStateT (sampleFromGrammar basicGrammar{expansions=p} tBool) 0 

instance NFData Comb where
    rnf c@(CApp l r t d n) = rnf l `seq` rnf r `seq` rnf t `seq` d `seq` rnf n `seq` c `seq` ()
    rnf c@(CLeaf n e t) = rnf n `seq` rnf e `seq` rnf t `seq` c `seq` () 
    rnf c@(CInnerNode t) = rnf t `seq` c `seq` ()

instance NFData Type where
instance NFData Expr where
instance NFData StdGen where

main = newStdGen >>= defaultMain . \g -> [bench "sample 0.1" $ nf (runEnv $ sampleFromBasicGrammar 0.1) g,
                                          bench "sample 0.2" $ nf (runEnv $ sampleFromBasicGrammar 0.1) g,
                                          bench "sample 0.3" $ nf (runEnv $ sampleFromBasicGrammar 0.1) g,
                                          bench "sample 0.4" $ nf (runEnv $ sampleFromBasicGrammar 0.1) g,
                                          bench "sample 0.5" $ nf (runEnv $ sampleFromBasicGrammar 0.1) g,
                                          bench "sample 0.6" $ nf (runEnv $ sampleFromBasicGrammar 0.4) g,
                                          bench "sample 0.7" $ nf (runEnv $ sampleFromBasicGrammar 0.8) g]
    
