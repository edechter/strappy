-- CombTrie.hs
-- Eyal Dechter

module CombTrie where

import Prelude hiding (lookup)
import qualified Prelude 
import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import qualified Data.Traversable as Traversable
import Data.Monoid (mappend, mempty)
import Data.Maybe
import Control.Monad (guard)

import CL



-- | Combinator Trie

data CombTrie a = CombTrie (Map.Map Comb a) (CombTrie (CombTrie a)) 
                | Spot 
                  deriving Eq

lookup :: CombTrie a -> Comb -> Maybe a
lookup (CombTrie m tr) (CApp l r []) = do {x <- lookup tr r; lookup x l} 
lookup (CombTrie m _) c = Map.lookup c m
lookup Spot _ = Nothing

empty :: CombTrie a
empty = Spot


single :: Comb -> a -> CombTrie a
single (CApp l r []) x = CombTrie Map.empty (single r (single l x))
single c x = CombTrie (Map.fromList $ [(c, x)]) empty

mergeWith :: (a -> a -> a) -> CombTrie a -> CombTrie a -> CombTrie a
mergeWith f (CombTrie m1 t1) (CombTrie m2 t2) 
    = CombTrie (Map.unionWith f m1 m2) (mergeWith (mergeWith f) t1 t2)
mergeWith f c@(CombTrie _ _) Spot = c
mergeWith f Spot c@(CombTrie _ _) = c
mergeWith f Spot Spot = Spot

merge = mergeWith (\a b -> b)

insertWith :: (a -> a -> a) -> Comb -> a -> CombTrie a -> CombTrie a
insertWith f c x m = mergeWith f m (single c x)

insert = insertWith (\a b -> b)

fromList :: [(Comb, a)] -> CombTrie a
fromList cs = foldl1 (mergeWith (\_ x -> x)) (map (\(a, b) -> single a b) cs)

length :: CombTrie a -> Int
length c = (Prelude.length . Fold.toList) c

keys :: CombTrie a -> [Comb]
keys Spot = []
keys (CombTrie v rest) 
        = do
            r <- keys rest
            let t = lookup rest r
            guard (isJust t)
            l <- (keys $ fromJust t)
            return $ CApp l r ""
          ++ Map.keys v

toList :: CombTrie a -> [a]
toList = Fold.toList

toAscList :: CombTrie a -> [(Comb, a)]
toAscList ct = [( k, fromJust $ lookup ct k) | k <- keys ct]


-- | Instances


instance (Eq a, Show a) => Show (CombTrie a) where
    show (CombTrie v rest) = show  v ++ " " ++ show rest
    show Spot = ""

instance Functor CombTrie where
    fmap f (CombTrie m ct) = CombTrie (fmap f m) (fmap (fmap f) ct)
    fmap f Spot = Spot

instance Fold.Foldable CombTrie where
    foldMap f (CombTrie v rest) 
            = Fold.foldMap f v  `mappend` (Fold.foldMap (Fold.foldMap f) rest)
    foldMap f Spot = mempty






                    

