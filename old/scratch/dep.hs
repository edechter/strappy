{-# Language MultiParamTypeClasses, FunctionalDependencies, GADTs, FlexibleInstances, UndecidableInstances, FlexibleContexts, OverlappingInstances #-}



data Zero = Zero
data Succ a = Succ a

instance Show Zero where
    show Zero = show 0

instance Nat a => Show (Succ a) where
    show (Succ x) = show $ (toNum x) + 1

type One = Succ Zero
type Two = Succ One

zero = Zero
one = Succ Zero

class Nat a where 
    toNum :: a -> Int
    fromNum :: Int -> a 

instance Nat Zero where
    toNum Zero = 0 

    fromNum 0 = Zero
    fromNum _ = undefined

instance Nat a => Nat (Succ a) where
    toNum (Succ a) = toNum a + 1
    fromNum k = Succ (fromNum $ k - 1)

instance Nat a => Num a where
    a + b = fromNum $ (toNum a) + (toNum b)
    a * b = fromNum $ (toNum a) * (toNum b)
    a - b = fromNum $ (toNum a) - (toNum b)
    negate a = fromNum $ negate $ toNum a
    abs = fromNum . abs . toNum
    signum = fromNum . signum . toNum
    fromInteger = fromNum . fromInteger

--class Add a b ab | a b -> ab, a ab -> b
--instance Add Zero a a
--instance (Add a b ab) => Add (Succ a) b (Succ ab)

data DList len a where
    Nil :: DList Zero a 
    Cons :: a -> DList l1 a ->  DList (Succ l1) a

instance Show v => (Show (DList len v)) where
    show Nil = "[]"
    show (Cons a b) = (show a) ++ ":" ++ (show b)

dHead :: DList (Succ a) b -> b 
dHead (Cons a xs) = a 

dTail :: Nat k => DList (Succ k) a -> DList k a
dTail (Cons x xs) = xs

dAppend :: DList k1 a -> DList k2 a -> DList (Add k1 k3) a
dAppend Nil xs = xs
--dAppend xs Nil = xs
--dAppend (Cons x xs) ys = Cons x (dAppend xs ys) 

--dReplicate n a = if n == 0 then Nil else Cons a (dReplicate (n-1) a)

nil = Nil :: DList Zero Int
--xs =  Cons 1 nil

x = Cons (1 :: Int) nil 



