module Data.Monoidal.Map.Internal
  ( MonoidalMap
  , showTree
  , empty
  , isEmpty
  , singleton
  , checkValid
  , insert
  , insertWith
  , lookup
  , lookupLE
  , lookupLT
  , lookupGE
  , lookupGT
  , findMin
  , findMax
  , foldSubmap
  , submap
  , fromFoldable
  , fromFoldableWith
  , fromFoldableWithIndex
  , toUnfoldable
  , toUnfoldableUnordered
  , delete
  , pop
  , member
  , alter
  , update
  , keys
  , values
  , union
  , unionWith
  , unions
  , intersection
  , intersectionWith
  , difference
  , isSubmap
  , size
  , filterWithKey
  , filterKeys
  , filter
  , mapMaybeWithKey
  , mapMaybe
  ) where

import Prelude

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Map.Internal as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, over2, unwrap, wrap)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple)
import Data.Unfoldable (class Unfoldable)

newtype MonoidalMap k v = MonoidalMap (Map.Map k v)

derive instance newtypeMonoidalMap :: Newtype (MonoidalMap k v) _
derive instance genericMonoidalMap :: Generic (MonoidalMap k v) _

derive newtype instance eq1MonoidalMap :: Eq k => Eq1 (MonoidalMap k)
derive newtype instance eqMonoidalMap :: (Eq k, Eq v) => Eq (MonoidalMap k v)
derive newtype instance ord1MonoidalMap :: Ord k => Ord1 (MonoidalMap k)
derive newtype instance ordMonoidalMap :: (Ord k, Ord v) => Ord (MonoidalMap k v)
derive newtype instance functorMonoidalMap :: Functor (MonoidalMap k)
derive newtype instance functorWithIdexMonoidalMap :: FunctorWithIndex k (MonoidalMap k)
derive newtype instance foldableMonoidalMap :: Foldable (MonoidalMap k)
derive newtype instance foldableWithIdexMonoidalMap :: FoldableWithIndex k (MonoidalMap k)
derive newtype instance traversableMonoidalMap :: Traversable (MonoidalMap k)
-- derive newtype instance alignMonoidalMap :: Ord k => Align (MonoidalMap k)

instance showMonoidalMap :: (Show k, Show v) => Show (MonoidalMap k v) where
  show m = genericShow m

instance semigroupMonoidalMap :: (Ord k, Semigroup v) => Semigroup (MonoidalMap k v) where
  append (MonoidalMap a) (MonoidalMap b) = MonoidalMap $ Map.unionWith (<>) a b

instance monoidMonoidalMap :: (Ord k, Monoid v) => Monoid (MonoidalMap k v) where
  mempty = MonoidalMap Map.empty

instance traversableWithIndexMonoidalMap :: TraversableWithIndex k (MonoidalMap k) where
  traverseWithIndex f (MonoidalMap m) = map MonoidalMap $ traverseWithIndex f m

showTree :: forall k v. Show k => Show v => MonoidalMap k v -> String
showTree = Map.showTree <<< unwrap

empty :: forall k v. MonoidalMap k v
empty = wrap Map.empty

isEmpty :: forall k v. MonoidalMap k v -> Boolean
isEmpty = Map.isEmpty <<< unwrap

singleton :: forall k a. k -> a -> MonoidalMap k a
singleton k a = MonoidalMap $ Map.singleton k a

checkValid :: forall k v. MonoidalMap k v -> Boolean
checkValid = Map.checkValid <<< unwrap

lookup :: forall k v. Ord k => k -> MonoidalMap k v -> Maybe v
lookup k = Map.lookup k <<< unwrap

lookupLE :: forall k v. Ord k => k -> MonoidalMap k v -> Maybe { key :: k, value :: v }
lookupLE k = Map.lookupLE k <<< unwrap

lookupLT :: forall k v. Ord k => k -> MonoidalMap k v -> Maybe { key :: k, value :: v }
lookupLT k = Map.lookupLT k <<< unwrap

lookupGE :: forall k v. Ord k => k -> MonoidalMap k v -> Maybe { key :: k, value :: v }
lookupGE k = Map.lookupGE k <<< unwrap

lookupGT :: forall k v. Ord k => k -> MonoidalMap k v -> Maybe { key :: k, value :: v }
lookupGT k = Map.lookupGT k <<< unwrap

findMax :: forall k v. MonoidalMap k v -> Maybe { key :: k, value :: v }
findMax = Map.findMax <<< unwrap

findMin :: forall k v. MonoidalMap k v -> Maybe { key :: k, value :: v }
findMin = Map.findMin <<< unwrap

foldSubmap :: forall k v m. Ord k => Monoid m => Maybe k -> Maybe k -> (k -> v -> m) -> MonoidalMap k v -> m
foldSubmap kmin kmax f = Map.foldSubmap kmin kmax f <<< unwrap

submap :: forall k v. Ord k => Maybe k -> Maybe k -> MonoidalMap k v -> MonoidalMap k v
submap kmin kmax = over MonoidalMap (Map.submap kmin kmax)

size :: forall k a. MonoidalMap k a -> Int
size = Map.size <<< unwrap

member :: forall k a. Ord k => k -> MonoidalMap k a -> Boolean
member k = Map.member k <<< unwrap

insert :: forall k v. Ord k => k -> v -> MonoidalMap k v -> MonoidalMap k v
insert k v = over MonoidalMap (Map.insert k v)

insertWith :: forall k v. Ord k => (v -> v -> v) -> k -> v -> MonoidalMap k v -> MonoidalMap k v
insertWith f k v = over MonoidalMap (Map.insertWith f k v)

delete :: forall k v. Ord k => k -> MonoidalMap k v -> MonoidalMap k v
delete k = over MonoidalMap (Map.delete k)

pop :: forall k v. Ord k => k -> MonoidalMap k v -> Maybe (Tuple v (MonoidalMap k v))
pop k = map (map wrap) <<< Map.pop k <<< unwrap

alter :: forall k v. Ord k => (Maybe v -> Maybe v) -> k -> MonoidalMap k v -> MonoidalMap k v
alter f k = over MonoidalMap (Map.alter f k)

update :: forall k v. Ord k => (v -> Maybe v) -> k -> MonoidalMap k v -> MonoidalMap k v
update f k = over MonoidalMap (Map.update f k)

fromFoldable :: forall f k v. Ord k => Foldable f => f (Tuple k v) -> MonoidalMap k v
fromFoldable = wrap <<< Map.fromFoldable

fromFoldableWith :: forall f k v. Ord k => Foldable f => (v -> v -> v) -> f (Tuple k v) -> MonoidalMap k v
fromFoldableWith f = wrap <<< Map.fromFoldableWith f

fromFoldableWithIndex :: forall f k v. Ord k => FoldableWithIndex k f => f v -> MonoidalMap k v
fromFoldableWithIndex = wrap <<< Map.fromFoldableWithIndex

toUnfoldable :: forall f k v. Unfoldable f => MonoidalMap k v -> f (Tuple k v)
toUnfoldable = Map.toUnfoldable <<< unwrap

toUnfoldableUnordered :: forall f k v. Unfoldable f => MonoidalMap k v -> f (Tuple k v)
toUnfoldableUnordered = Map.toUnfoldableUnordered <<< unwrap

keys :: forall k v. MonoidalMap k v -> List k
keys = Map.keys <<< unwrap

values :: forall k v. MonoidalMap k v -> List v
values = Map.values <<< unwrap

unionWith :: forall k v. Ord k => (v -> v -> v) -> MonoidalMap k v -> MonoidalMap k v -> MonoidalMap k v
unionWith f = over2 MonoidalMap (Map.unionWith f)

union :: forall k v. Ord k => MonoidalMap k v -> MonoidalMap k v -> MonoidalMap k v
union = unionWith const

unions :: forall k v f. Ord k => Foldable f => f (MonoidalMap k v) -> MonoidalMap k v
unions = foldl union empty

intersectionWith :: forall k a b c. Ord k => (a -> b -> c) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k c
intersectionWith f (MonoidalMap m1) (MonoidalMap m2) = MonoidalMap (Map.intersectionWith f m1 m2)

intersection :: forall k a b. Ord k => MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k a
intersection = intersectionWith const

difference :: forall k v w. Ord k => MonoidalMap k v -> MonoidalMap k w -> MonoidalMap k v
difference (MonoidalMap m1) (MonoidalMap m2) = MonoidalMap (Map.difference m1 m2)

isSubmap :: forall k v. Ord k => Eq v => MonoidalMap k v -> MonoidalMap k v -> Boolean
isSubmap (MonoidalMap m1) (MonoidalMap m2) = Map.isSubmap m1 m2

filterWithKey :: forall k v. Ord k => (k -> v -> Boolean) -> MonoidalMap k v -> MonoidalMap k v
filterWithKey p = over MonoidalMap (Map.filterWithKey p)

filterKeys :: forall k. Ord k => (k -> Boolean) -> MonoidalMap k ~> MonoidalMap k
filterKeys p = over MonoidalMap (Map.filterKeys p)

filter :: forall k v. Ord k => (v -> Boolean) -> MonoidalMap k v -> MonoidalMap k v
filter p = over MonoidalMap (Map.filter p)

mapMaybeWithKey :: forall k a b. Ord k => (k -> a -> Maybe b) -> MonoidalMap k a -> MonoidalMap k b
mapMaybeWithKey f = over MonoidalMap (Map.mapMaybeWithKey f)

mapMaybe :: forall k a b. Ord k => (a -> Maybe b) -> MonoidalMap k a -> MonoidalMap k b
mapMaybe = mapMaybeWithKey <<< const