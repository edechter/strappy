
-- | This module supplies the CombMap alias for Map Comb by defining a
-- Hashable instance for Comb.
module Strappy.CombMap 
(

 CombMap
                    -- * Operators
                    ,(!), (\\)

                    -- * Query
                    , null
                    , size
                    , member
                    , notMember
                    , lookup
                    , findWithDefault

                    -- * Construction
                    , empty
                    , singleton

                    -- ** Insertion
                    , insert
                    , insertWith, insertWithKey, insertLookupWithKey

                    -- ** Delete\/Update
                    , delete
                    , adjust
                    , adjustWithKey
                    , update
                    , updateWithKey
                    , updateLookupWithKey
                    , alter

                    -- * Combine

                    -- ** Union
                    , union
                    , unionWith
                    , unionWithKey
                    , unions
                    , unionsWith

                    -- ** Difference
                    , difference
                    , differenceWith
                    , differenceWithKey

                    -- ** Intersection
                    , intersection
                    , intersectionWith
                    , intersectionWithKey

                    -- * Traversal
                    -- ** Map
                    , map
                    , mapWithKey
                    , mapAccum
                    , mapAccumWithKey

                    -- ** Fold
                    , fold
                    , foldWithKey

                    -- * Conversion
                    , elems
                    , keys
                    , keysSet
                    , assocs

                    -- ** Lists
                    , toList
                    , fromList
                    , fromListWith
                    , fromListWithKey

                    -- * Filter
                    , filter
                    , filterWithKey
                    , partition
                    , partitionWithKey

                    , mapMaybe
                    , mapMaybeWithKey
                    , mapEither
                    , mapEitherWithKey

                    -- * Submap
                    , isSubmapOf, isSubmapOfBy
                    , isProperSubmapOf, isProperSubmapOfBy

, showCombMap
) 
where

import Prelude hiding (lookup,map,filter,null)
import qualified Prelude as Prelude
import Data.HashMap 
import Data.Hashable

import Strappy.CL

type CombMap a = Map Comb a

instance Hashable Comb where
    hash (CLeaf name _ _ _) = hash name
    hash CApp{lComb=cl, rComb=cr} = hash cl `hashWithSalt` hash cr

showCombMap :: Show a => CombMap a -> String
showCombMap cm = unlines $ Prelude.map (\(k, a) -> show k ++ ": " ++ show a ) 
                 $ toList cm

