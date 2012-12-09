-- MemoType.hs

module MemoType where

import Debug.Trace 

import qualified Data.Map as DMap

import Type


data TypeTrie v = TypeTrie [(Type, v)] (TypeTrie (TypeTrie v)) deriving Show

applyTypeTrie :: TypeTrie v ->  (Type -> Maybe v)
applyTypeTrie (TypeTrie tl tf) (Map l r) = do
  a <- applyTypeTrie tf l
  applyTypeTrie a r
applyTypeTrie (TypeTrie tl tf) leaf = lookup leaf tl

tabulateTypeTrie :: (Type -> v) -> TypeTrie v
tabulateTypeTrie f = TypeTrie leafMap (tabulateTypeTrie $ \l
                                       -> tabulateTypeTrie $ \r -> f (Map l r))
                     where leafMap = [(Rtype, f Rtype), 
                                                 (Btype, f Btype)
                                                 ]  ++ 
                                     [(TVar i, f (TVar i)) | i <- [0..]]

memoType = applyTypeTrie . tabulateTypeTrie