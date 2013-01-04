-- TestType.hs

module TestType where

import Test.QuickCheck
import Test.HUnit
import Control.Monad
import StdLib
import CL
import qualified Data.Set as Set

import Debug.Trace

import Type

instance Arbitrary TyVar where
    arbitrary = liftM2 TyVar (liftM (enumId . abs) arbitrary) (return Star)

prop_mkTVar :: Int -> Bool
prop_mkTVar i = enumId i == k
    where (TVar (TyVar k _)) = mkTVar i

instance Arbitrary Type where
    arbitrary = sized typegen

typegen 0 = oneof [ return tInt, 
                    liftM TVar arbitrary
                    ]
typegen n | n > 0 = oneof [ liftM2 (->-) subtype subtype]
          where subtype = typegen (n `div` 2)
                    
prop_applyNullSubst :: Type -> Bool
prop_applyNullSubst t = (apply nullSubst t) == t

prop_mergeSameSubst :: Subst -> Bool
prop_mergeSameSubst s = merge s s == Just (s ++ s)

test_mgu1 = TestCase $  assertEqual "test mgu" 
            (mgu tInt tInt) (Just nullSubst)                
prop_mgu2 t = 
    mgu ((TVar t) ->- tInt) (tChar ->-  tInt) == Just [(t, tChar)]

prop_type :: Type -> Bool
prop_type t = (trace $ show t) $ True

-- test_unify = TestList [test1]
--     where test1 = TestCase $ assertBool "test_unify 1" 
--                   (not $ runTI $ unify' (t0 ->- t0) (t6 ->- t7 ->- t8))
                  

-- test_freshInst = TestList [test1, test2, test3, test4]
--     where t = t0 ->- (t1 ->- t2)
--           t' = t3 ->- (t4 ->- t5)
--           t''= t6 ->- (t7 ->- t8)
--           s = t0 ->- t1 ->- t0
--           s' = t2 ->- t3 ->- t2
--           test1 = TestCase $ assertEqual "test_freshInst 1"
--                   t' (runTI $ freshInst t) 
--           test2 = TestCase $ assertEqual "test_freshInst 2"
--                   t' (runTI $ newTVar Star >> freshInst t) 
--           test3 = TestCase $ assertEqual "test_freshInst 3"
--                   t'' (runTI f) 
--               where f = do
--                       sequence_ (take 6 $ repeat (newTVar Star))
--                       freshInst t
--           test4 = TestCase $ assertEqual "test_freshInst 4"
--                   s' (runTI $ freshInst s)

-- test_freshInstComb = TestList [test1]
--     where cTimes = dOp2C "*" (*)
--           c = CApp cS cTimes [] 0
--           tS = ((t3 ->- (t4 ->- t5)) ->- ((t3 ->- t4) ->- (t3 ->- t5)))
--           test1 = TestCase $ assertEqual "test_freshInstComb 1"
--                   tS (runTI $ freshInstComb cS >>= typeCheck)
                  
          
          
                      
          

