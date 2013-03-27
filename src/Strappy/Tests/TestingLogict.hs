-- TestingLogict.hs

import Control.Monad
import Control.Monad.Logic
import Control.Monad.State

type LogicState = LogicT (State Int)


incr :: State Int Int
incr = do i <- get
          put (i + 1)
          i' <- get
          return i'

incr' = lift incr 
y =  incr' >> (incr' `mplus` incr')

main = do
  putStrLn $ show (fst $ runState (observeAllT y) 0)

