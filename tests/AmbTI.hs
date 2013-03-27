-- AmbTI.hs
{-# Language TypeSynonymInstances, FlexibleInstances  #-} 

import Prelude hiding  (map, concat)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Debug.Trace
import Type

data Amb m a = Cons (m a) (Amb m a) | Empty


infixr 4 <:>
(<:>) = Cons

map :: Monad m => (a -> b) -> Amb m a -> Amb m b
map f (Cons m xs) = Cons y (map f xs)
    where y = do a <- m
                 return $ f a
map f Empty = Empty

unpack :: (MonadPlus m) => m (Amb m a) -> Amb m a 
unpack m = let first = join $ do t <- m
                                 case t of
                                   (Cons x ys) -> return x
                                   Empty -> mzero
               rest =  do (Cons x ys) <- m
                          return ys
           in Cons first (unpack rest)
                    

concat :: (MonadPlus m) => Amb m (Amb m a) -> Amb m a
concat (Cons m xs)  = (unpack m) `mplus` (concat xs)
concat  Empty = Empty
             
instance (MonadPlus m) => Monad (Amb m) where
    return x = Cons (return x) Empty
    xs >>= f = let yss = map f xs
               in concat yss

instance MonadPlus m => MonadPlus (Amb m) where
    mzero = Empty
    (Cons m xs) `mplus` ys = Cons m (xs `mplus` ys)
    Empty `mplus` ys = ys

instance MonadTrans Amb where
    lift m = Cons m Empty

type AmbTI = Amb TI 

instance Show a => Show (AmbTI a) where
    show m = (show .  toList) m

toList :: AmbTI a -> [a]
toList Empty = []
toList (n `Cons` xs) = (runTI n : toList xs)



a = newTVar Star
b = lift $ newTVar Star
c = b >> (a <:> ((a >> a) <:> Empty))
out = c


type AmbState = Amb (State Int)

incr :: State Int Int
incr = do i <- get
          put (i + 1)
          i' <- get
          return i'



-- instance Show a => Show (Amb (State Int) a) where
--     show m = (show .  toList) m

-- toList :: Amb (State Int) a -> [a]
-- toList Empty = []
-- toList (n `Cons` xs) = (runState n 0 : toList xs)


-- x = (list $ incr) >> (incr <:> incr <:> Empty)
-- y = (list $ incr) >> (incr <:> (incr >> incr) <:> Empty)

-- main = do
--   putStr $ show x -- | should be [2, 2]
--   putStr $ show y -- | should be [2, 3]



