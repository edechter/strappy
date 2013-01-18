-- Expr.hs
{-# Language BangPatterns #-}

module Expr where

import Debug.Trace

import Type


-- define an expression
data Expr  = App Expr Expr
           | Func (Expr -> Expr)
           | Lam Expr
           | N Int
           | C Char
           | B Bool
           | Var Id -- variable indexed by de'brujn notation
           | Const String
           | ExprError String 

instance Show Expr where
    show (Func _) = "<function>"
    show (App left right) = "(" ++ show left ++ " " ++ show right ++ ")"
    show (N i) = show i
    show (C c) = show c
    show (B t) = show t -- boolean
    show (Const name) = name
    show (Lam expr) = 
        "( L: " ++ show expr ++ ")"
    show (Var id) = "var_" ++ show id
    show (ExprError s) = "Expr Error: " ++ s

instance Eq Expr where
    (App l1 r1) == (App l2 r2) = (l1 == l2) && (r1 == r2)
    (Lam e) == (Lam f) = (e == f)
    (Var i) == (Var j) = i == j
    _ == _ = False


toFloat :: Expr -> Double
toFloat e = error $ "expr cannot be converted to float: " ++ show e

isRedex :: Expr -> Bool
{-# INLINE isRedex #-}
isRedex (App (Const _ ) _) = False
isRedex (App _ _) = True
isRedex _ = False

reduce :: Expr -> Expr
{-# INLINE reduce #-} 
reduce (App err@(ExprError s) _) = err
reduce (App _ err@(ExprError s)) = err

reduce x@(App (Lam e) f) = let f' = reduce f 
                           in case f' of 
                                err@(ExprError _) -> err
                                otherwise -> reduce $ substitute f' e

reduce e@(App (Func f) a)  = let z = (reduce a) 
                             in case z of 
                                  err@(ExprError _) -> err
                                  otherwise -> z `seq` reduce $ f z
reduce e@(App (Const _) _) =  e
reduce x@(App a b) | isRedex a =  let a' = reduce a 
                                  in case a' of
                                       err@(ExprError _) -> err
                                       otherwise -> reduce $ App a' b
                   | otherwise = x

reduce e = e

substitute e (App a b) = App a' b' where a' = substitute e a
                                         b' = substitute e b
substitute e (Lam f) = Lam $ substitute e f
substitute e a@(Func _) = a
substitute e v@(Var "v0") = e
substitute e v@(Var _) = v

-- | implement reduction with a step limit
reduceWithLimit :: Int -> Expr -> Maybe Expr
{-# INLINE reduceWithLimit #-}
reduceWithLimit 0 e = (trace $ "Reduction limit reached.") $ Nothing
reduceWithLimit i x@(App (Lam e) f) = 
    do f' <- reduceWithLimit (i-1) f 
       reduceWithLimit (i-1) $ substitute f' e
reduceWithLimit i  e@(App (Func f) a)  = 
    do  z <- reduceWithLimit (i-1) a
        z `seq` reduceWithLimit (i-1) $ f z
reduceWithLimit i x@(App a b) = 
    do l <- reduceWithLimit (i-1 ) a
       r <- reduceWithLimit (i-1) b
       reduceWithLimit (i-1) (App l r)
reduceWithLimit i e = Just e


                                    
