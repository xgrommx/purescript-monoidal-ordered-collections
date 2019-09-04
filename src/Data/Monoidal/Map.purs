module Data.Monoidal.Map
  ( module Data.Monoidal.Map.Internal
  , keys
  , groupByMonoidOf
  , groupByMonoidOf'
  , groupByMonoidOf''
  ) where

import Prelude

import Data.Lens as L
import Data.Map as Map
import Data.Monoidal.Map.Internal (MonoidalMap, alter, checkValid, delete, empty, filter, filterKeys, filterWithKey, findMax, findMin, foldSubmap, fromFoldable, fromFoldableWith, fromFoldableWithIndex, insert, insertWith, isEmpty, isSubmap, lookup, lookupGE, lookupGT, lookupLE, lookupLT, member, pop, showTree, singleton, size, submap, toUnfoldable, toUnfoldableUnordered, union, unionWith, unions, intersection, intersectionWith, difference, update, values, mapMaybeWithKey, mapMaybe)
import Data.Newtype (unwrap)
import Data.Profunctor.Strong ((&&&), (***))
import Data.Set (Set)
import Data.Tuple (Tuple, uncurry)

keys :: forall k v. MonoidalMap k v -> Set k
keys = Map.keys <<< unwrap

-- move it to profunctor-lenses-extra

groupByMonoidOf 
  :: forall s m v k
  . Monoid m 
  => Ord k 
  => L.Fold (MonoidalMap k m) s s (Tuple k m) (Tuple k v)
  -> s
  -> MonoidalMap k m
groupByMonoidOf l = L.foldMapOf l (uncurry singleton)

groupByMonoidOf' 
  :: forall s m v k
  . Monoid m 
  => Ord k
  => L.Fold' (MonoidalMap k m) s v 
  -> (v -> k) 
  -> (v -> m) 
  -> s 
  -> MonoidalMap k m
groupByMonoidOf' l f g = groupByMonoidOf (l <<< L.to (f &&& g))

groupByMonoidOf'' 
  :: forall s m v k k'
  . Monoid m
  => Ord k' 
  => L.Fold' (MonoidalMap k' m) s (Tuple k v) 
  -> (k -> k') 
  -> (v -> m) 
  -> s 
  -> MonoidalMap k' m
groupByMonoidOf'' l f g = groupByMonoidOf (l <<< L.to (f *** g))