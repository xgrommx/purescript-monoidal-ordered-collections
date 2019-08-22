module Data.Monoidal.Map
  ( module Data.Monoidal.Map.Internal
  , keys
  ) where

import Prelude

import Data.Map as Map
import Data.Monoidal.Map.Internal (MonoidalMap, alter, checkValid, delete, empty, filter, filterKeys, filterWithKey, findMax, findMin, foldSubmap, fromFoldable, fromFoldableWith, fromFoldableWithIndex, insert, insertWith, isEmpty, isSubmap, lookup, lookupGE, lookupGT, lookupLE, lookupLT, member, pop, showTree, singleton, size, submap, toUnfoldable, toUnfoldableUnordered, union, unionWith, unions, intersection, intersectionWith, difference, update, values, mapMaybeWithKey, mapMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)

keys :: forall k v. MonoidalMap k v -> Set k
keys = Map.keys <<< unwrap