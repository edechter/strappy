{-# Language TypeOperators #-}

module Observer where


data Base d a  = Base (Maybe d) a
data App d a =  App (Maybe d, a) -> (Maybe d, b)
data Comb d = Comb (d -> d -> d)

pure :: a -> Base d a
pure a = Base Nothing a

lift :: Comb d -> (a -> b) -> App d a b
lift (Comb c) f = App g
	where g (d)
app :: Comb d -> App d a b -> Base d a -> Base d b


--data a :->: b = M (Maybe String, a -> M a b)


--observe :: (M a b) -> Maybe String
--observe (M (s, _)) = s

--next :: (M a b) -> (a -> M a b)
--next (M (_, f)) = f

--app :: M a b -> a -> M a b
--M (s, f) `app` a = M (combine s s', g) where M (s', g) = f a

--combine :: Maybe String -> Maybe String -> Maybe String
--combine (Just x) (Just y) = Just $ x ++ y
--combine (Just x) Nothing = Just x
--combine Nothing (Just x) = Just x
--combine Nothing Nothing = Nothing

--lift :: (a -> String) -> M a b
--lift f = M (Nothing, \a -> M (Just $ f a, next $ lift f))

--chain :: M a b -> M a b -> M a b
--chain m1@(M (s1, f1)) (M (s2, f2)) = let f3 a = M (s', f2)  where s' = observe $ m1 `app` a in M (s1, f3)

--repeatM :: 
									

--f = lift $ \i -> ["abc" !! (i `mod` 3)]
--g = lift $ \i -> ["xyz" !! (i `mod` 3)]
--chain :: (a -> M b) -> (b -> )

--lift :: (a -> String) -> M a (b -> b)
--lift f = (Nothing, f') where f' = \a -> (Just (f a), id)

--app :: M a (M c d) -> a -> M c d
--(s1, f1) `app` a = (s3, snd f2) where (s2, f2) = f1 a
--                                      s3 = s1 `combine` s3

--chain :: M a b -> M c d -> M a (M c d)
--(s1, f1) `chain` (s2, f2) = (s1, f3) where f3 a = fst f1 a


----compose :: M a -> M a -> M a
----compose (M (s1, f2)) (M (s2, f2)) = M (combine s1 s2, )
