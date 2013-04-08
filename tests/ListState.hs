-- ListState.hs


import ListTDoneRight

import Control.Monad.State

type ListState = ListT (State Int)

-- | Generate all strings of up to length d that consist of elements
-- from an alphabet ['a', 'b', 'c']

toList :: ListState a -> [a]
toList x = case (runState (runListT x) 0) of
             (Just y, _) -> ((fst y) : toList (snd y))
             (Nothing, _) -> []


incrState :: State Int ()
incrState = do 
  s <- get
  put (s+1)

enum :: Int -> ListState Int
enum 0 = liftList ['a','b','c'] 
enum d = do
  lift incrState
  enum (d-1)

