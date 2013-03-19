-- TestTrans2.hs

import Control.Monad.Error
import Control.Monad.Trans.Class
import Control.Monad.List

type MError m a = m (Either String) a

data MyTrans m a = MyTrans {runMyTrans :: (MError m) a}

instance Monad m => Monad (MyTrans m) where
    return  = MyTrans . return 
    m >>= f = MyTrans $ do x <- runMyTrans m
                           runMyTrans (f x)

instance MonadTrans MyTrans where
    lift m = MyTrans $ lift m
    
instance MonadPlus m => MonadPlus (MyTrans m) where
    mzero = MyTrans $ lift mzero
    -- Attempt #1 (Only reveals the first element)
    m `mplus` n = MyTrans $ (runMyTrans m) `mplus` (runMyTrans n) 
    -- Attempt #2 (Incomplete: see undefined statements)
--     m `mplus` n = MyTrans $ 
--                   lift $ do a <- runErrorT $ runMyTrans m
--                             b <- runErrorT $ runMyTrans n
--                             case a of 
--                               Right r 1
--                                   -> case b of 
--                                        Left _ -> undefined
--                                        Right t -> return r `mplus` return t

                            
type ListWError =  ListT (Either String)
type MyMonad = MyTrans  
    
x = return 1 :: MyMonad Int
y = MyTrans $ throwError "Error" :: MyMonad Int

z = x `mplus` y

main = do
  print $ (runErrorT $ runMyTrans z) -- should be [Right 1, Left "Error"]

                                                    
                                                    
                                      
                                  
                           
    