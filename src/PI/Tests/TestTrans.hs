-- TestTrans.hs

import Control.Monad.Error
import Control.Monad.Trans.Class

data MyTrans m a = MyTrans {runMyTrans :: ErrorT String m a}

instance Monad m => Monad (MyTrans m) where
    return  = MyTrans . lift . return 
    m >>= f = MyTrans $ do x <- runMyTrans m
                           runMyTrans (f x)

instance MonadTrans MyTrans where
    lift m = MyTrans $ lift m
    
instance MonadPlus m => MonadPlus (MyTrans m) where
    mzero = MyTrans $ lift mzero
    -- Attempt #1 (Only reveals the first element)
--    m `mplus` n = MyTrans $ (runMyTrans m) `mplus` (runMyTrans n) 
    -- Attempt #2 (Incomplete: see undefined statements)
    m `mplus` n = MyTrans $ 
                  lift $ do a <- runErrorT $ runMyTrans m
                            b <- runErrorT $ runMyTrans n
                            case a of 
                              Left _ -> case b of
                                          Left _ -> mzero
                                          Right t -> return t
                              Right r 
                                  -> case b of 
                                       Left _ -> return r 
                                       Right t -> return r `mplus` return t

                            

type MyMonad = MyTrans [] 
    
x = return 1 :: MyMonad Int
y = MyTrans $ throwError "Error" :: MyMonad Int

z = x `mplus` y

main = do
  print $ (runErrorT $ runMyTrans z) -- should be [Right 1, Left "Error"]

                                                    
                                                    
                                      
                                  
                           
    