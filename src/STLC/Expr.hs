-- Expr.hs

module Expr where

type Info = Int
type Context = [(String, Binding)]
data Binding = NameBind

data Term = TVar {info :: Info -- ^ aux. info
                 , index :: Int -- ^  de Bruijn index
                 , ctxsize :: Int
                 } 
          | TAbs {info :: Info
                 , hint :: String
                 , term :: Term}
          | TApp {info :: Info
                 , terml :: Term
                 , termr :: Term} deriving (Show, Eq)

printTerm :: Context -> Term -> String
printTerm ctx (TVar inf x n) = if length ctx == n
                               then show $ indexToName inf ctx  x
                               else show "[bad index]"

(!!!)                  :: [a] -> Int -> Maybe a
xs       !!! n | n < 0 =  Nothing
[]       !!! _         =  Nothing
(x : _)  !!! 0         =  Just x
(_ : xs) !!! n         =  xs !!! (n - 1)

indexToName :: Info -> Context -> Int -> String
indexToName inf ctx i = case ctx !!! i of
                          Just (name, binding) -> name
                          Nothing -> "Variable lookup failure."

-- Shifting --
-- ^ a d-place shift (see Pierce 6.2.1)
shift :: Int  -- ^ cutoff
      -> Int -- ^ shift
      -> Term
      -> Term
shift c d t@(TVar _ k _ ) | k < c     =  t{index = k}
                          | otherwise   =  t{index = k + d}
shift c d t@(TAbs _ _ t') = t{term = shift (c+1) d t'}
shift c d t@(TApp _ tl tr) = t{terml = shift c d tl, termr = shift c d tr}

shift1 = shift 0 1

-- Substitute -- 
subst :: Int -- ^ de Bruijn index
      -> Term -- ^ replacement
      -> Term -- ^ body
      -> Term
subst j s t@(TVar _ i _) | i == j    = s
                         | otherwise = t
subst j s t@(TAbs _ _ t') = t{term = subst (j+1) (shift1 s) t'}
subst j s t@(TApp _ tl tr) = t{ terml = subst j s tl,
                                termr = subst j s tr}




                  