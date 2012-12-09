-- TestType.hs

import Test.QuickCheck
import Test.HUnit
import Control.Monad
import qualified Data.Set as Set

import Debug.Trace

import Type


instance Arbitrary TyVar where
    arbitrary = liftM TyVar $ liftM getPositive arbitrary

prop_mkTVar :: Int -> Bool
prop_mkTVar i = i == k
    where (TVar (TyVar k)) = mkTVar i

instance Arbitrary Type where
    arbitrary = sized typegen

typegen 0 = oneof [ return Rtype, 
                    return Btype,
                    return TyIntList,
                    liftM TVar arbitrary
                    ]
typegen n | n > 0 = oneof [ liftM2 Map subtype subtype,
                            liftM2 Prod subtype subtype,
                            liftM2 Sum subtype subtype]
          where subtype = typegen (n `div` 2)

    
                    
prop_applyNullSubst :: Type -> Bool
prop_applyNullSubst t = (apply nullSubst t) == t

test_tv = TestCase $ assertEqual "test tv" (Set.fromList ts) (Set.fromList (tv t))
          where ts = [t1,t2,t3]
                [t1, t2, t3] = map TyVar $ [1,2,3]
                t = Map (Map (TVar t3) TyIntList) (Prod (TVar t2) (TVar t1))

prop_mergeSameSubst :: Subst -> Bool
prop_mergeSameSubst s = merge s s == Just (s ++ s)

test_mgu1 = TestCase $  assertEqual "test mgu" 
            (mgu Rtype Rtype) (Just nullSubst)                
prop_mgu2 t = 
    mgu (Map (TVar t) Rtype) (Map Btype Rtype) == Just [(t, Btype)]

prop_type :: Type -> Bool
prop_type t = (trace $ show t) $ True