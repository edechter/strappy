-- TestStateT.hs

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.List


incr = modify (+1)
sample1 = incr `mplus` incr
sample2 = incr `mplus` (incr >> incr)
sample3 = incr >> ((incr >> incr) `mplus` incr)

monomorphicExecStateT :: StateT Int [] a -> Int -> [Int]
monomorphicExecStateT = execStateT

-- | type inference monad


data Type = Type Int deriving Show
data Subst = Subst Int deriving Show
type TIError = ErrorT String

data TypeInfT m a 
    = TypeInfT { runTypeInfT :: (Subst -- ^ current set of substitutions 
                -> Int -- ^ increment integer for new types variables
                -> TIError m (Subst, Int, a))}

instance (Monad m) => Monad (TypeInfT m) where
    return x = TypeInfT (\c -> \i -> (lift . return) (c, i, x))
    m >>=  f = TypeInfT $ \s -> \i -> do
                 (s', i', y) <- runTypeInfT m s i
                 runTypeInfT (f y) s' i'

                 
instance MonadTrans TypeInfT where
    lift m = TypeInfT $ \s -> \i ->  lift $ do 
                               a <- m
                               return (s, i , a)

instance (MonadPlus m) => MonadPlus (TypeInfT m) where
    mzero = TypeInfT $ \_ -> \_ ->  lift $ mzero
    m `mplus` n = TypeInfT $ \s i -> 
                  lift $ do a <- runErrorT $ runTypeInfT m s i
                            b <- runErrorT $ runTypeInfT n s i
                            case a of 
                              Left _ -> case b of
                                          Left _ -> mzero
                                          Right t -> return t
                              Right r 
                                  -> case b of 
                                       Left _ -> return r 
                                       Right t -> return r `mplus` return t

newTVar :: (Monad m) => TypeInfT m Type                             
newTVar = TypeInfT (\s i -> return (s, (i+1), Type i))

throwTIError :: (Monad m) => TypeInfT m a
throwTIError = TypeInfT $ \_ _ -> throwError "error"

type ListTI = TypeInfT []

runListTI :: TypeInfT [] a -> Subst -> Int -> [Either String  (Subst, Int, a)]
runListTI m s i = runErrorT $ runTypeInfT m s i                  

sample4 = throwTIError >> (newTVar `mplus` (newTVar))


main = do
    print (monomorphicExecStateT sample1 0) -- [1, 1]
    print (monomorphicExecStateT sample2 0) -- [1, 2]
    print (monomorphicExecStateT sample3 0) -- [1, 2]
    print (runListTI sample4 (Subst 0) 0)

