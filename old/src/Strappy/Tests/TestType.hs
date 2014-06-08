-- TestType.hs

module TestType where

import Control.Monad

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import StdLib
import CL
import qualified Data.Set as Set
import Debug.Trace

import Type

typeTestGroup = testGroup "Type Testing Group" [
                testProperty "mkTVar preserves assigned integer " 
                             prop_mkTVar,
                testProperty "applying null substitution doesn't do anything" 
                             prop_applyNullSubst,
                testProperty "merging 2 identical substitutions just concatenates them"
                             prop_mergeSameSubst,
                testCase "mgu of tInt w tInt is empty" test_mgu1,
                testCase "mgu of tInt w tBool is Nothing" test_mgu2,
                testCase "mgu 3" test_mgu3,
                testCase "mgu 4" test_mgu4,
                testCase "mgu 5" test_mgu5,
                testCase "mgu 6" test_mgu6
                
            ]


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

test_mgu1 = assertEqual "test mgu" 
            (mgu tInt tInt) (Just nullSubst)                
test_mgu2 = mgu tInt tBool @=? Nothing
test_mgu3 = mgu (t0 ->- t0) (tInt ->- t1) @=? Just [(tv0, tInt), (tv1, tInt)]
            where t0 = mkTVar 0
                  t1 = mkTVar 1
                  tv0 = TyVar (enumId 0) Star
                  tv1 = TyVar (enumId 1) Star
test_mgu4 = mgu (t0 ->- t1) (tInt ->- t1) @=? Just [(tv0, tInt)]
            where t0 = mkTVar 0
                  t1 = mkTVar 1
                  tv0 = TyVar (enumId 0) Star
                  tv1 = TyVar (enumId 1) Star

test_mgu5 = mgu (t0 ->- t1) (tInt ->- (t2 ->- tInt)) @=? Just [(tv0, tInt), (tv1, (t2 ->- tInt))]
            where t0 = mkTVar 0
                  t1 = mkTVar 1
                  t2 = mkTVar 2
                  tv0 = TyVar (enumId 0) Star
                  tv1 = TyVar (enumId 1) Star
test_mgu6 = mgu (t0 ->- t0) (tInt ->- (t2 ->- tInt)) @=? Nothing


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
                  
          
          
                      
          

