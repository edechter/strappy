-- CombMap.hs

module CombMap 
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
                    , isProperSubmapOf, isProperSubmapOfBy) 
where

import Prelude hiding (lookup,map,filter,null)
import Data.HashMap 
import Data.Hashable

import CL

type CombMap a = Map Comb a

instance Hashable Comb where
    hash (CNode name _ _) = hash name
    hash (CApp rComb lComb _ _) = hash rComb `hashWithSalt` lComb

