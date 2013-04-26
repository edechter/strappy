

data CL = S | B | C | I | Symb String | CL :> CL
instance Show CL where
    show S = "S"
    show B = "B"
    show C = "C"
    show I = "I"
    show (Symb s) =  s
    show (x :> y) = "(" ++ show x ++ " " ++ show y ++ ")"


infixl :> 

redex :: CL -> Bool
redex (S :> _ :> _ :> _) = True
redex (B :> _ :> _ :> _) = True
redex (C :> _ :> _ :> _) = True
redex (I :> _) = True
redex _ = False

reduce :: CL -> CL
reduce (S :> f :> g :> x) = reduce $ (f :> x) :> (g :> x)
reduce (B :> f :> g :> x) = reduce $ f :> (g :> x)
reduce (C :> f :> g :> x) = reduce $ (f :> x) :> g
reduce (I :> x) = reduce x
reduce (x :> y) = if redex x then reduce (reduce x :> reduce y) 
                  else if redex y then reduce (reduce x :> reduce y)
                       else (reduce x :> reduce y)
reduce c = c
