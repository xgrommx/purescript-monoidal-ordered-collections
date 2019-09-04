module Data.Multiset where

import Data.Char.Unicode
import Prelude

import Data.Array as A
import Data.Foldable (class Foldable, fold)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Lens as L
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Monoidal.Map as M
import Data.Newtype as N
import Data.Ord.Down (Down(..))
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), snd)

type MultiSet a = M.MonoidalMap a (Additive Int)

fromFoldable :: forall f a. Ord a => Foldable f => f a -> MultiSet a 
fromFoldable = M.groupByMonoidOf' L.folded identity (const (Additive 1))

fromOccurFoldable :: forall f a. Ord a => Foldable f => f (Tuple a Int) -> MultiSet a 
fromOccurFoldable = M.groupByMonoidOf'' L.folded identity (const (Additive 1))

size :: forall a. Ord a => MultiSet a -> Int
size = N.unwrap <<< fold

distinctSize :: forall a. MultiSet a -> Int
distinctSize = M.size

occur :: forall a. Ord a => a -> MultiSet a -> Int
occur x = maybe 0 N.unwrap <<< M.lookup x

singleton :: forall a. a -> MultiSet a
singleton x = M.singleton x (Additive 1)

insert :: forall a. Ord a => a -> MultiSet a -> MultiSet a
insert x = M.insertWith (<>) x (Additive 1)

insertMany :: forall a. Ord a => a -> Int -> MultiSet a -> MultiSet a
insertMany x n
  | n <  0 = M.update (deleteN (Additive $ negate n)) x
  | n == 0 = identity
  | otherwise =  M.insertWith (<>) x (Additive n)

deleteN :: (Additive Int) -> (Additive Int) -> Maybe (Additive Int)
deleteN (Additive n) (Additive m)
  | m <= n    = Nothing
  | otherwise = Just (Additive (m - n))

delete :: forall a. Ord a => a -> MultiSet a -> MultiSet a
delete = M.update (deleteN (Additive 1))

deleteMany :: forall a. Ord a => a -> Int -> MultiSet a -> MultiSet a
deleteMany x n = insertMany x (negate n)

deleteAll :: forall a. Ord a => a -> MultiSet a -> MultiSet a
deleteAll x = M.delete x

findMin :: forall a. MultiSet a -> Maybe a
findMin m = _.key <$> M.findMin m

findMax :: forall a. MultiSet a -> Maybe a
findMax m = _.key <$> M.findMax m

union :: forall a. Ord a => MultiSet a -> MultiSet a -> MultiSet a
union = append

unions :: forall f a. Ord a => Foldable f => f (MultiSet a) -> MultiSet a
unions = fold

maxUnion :: forall a. Ord a => MultiSet a -> MultiSet a -> MultiSet a
maxUnion m1 m2 = M.unionWith max m1 m2

intersection :: forall a. Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersection m1 m2 = M.intersectionWith min m1 m2

filter' :: forall a. Ord a => (a -> Boolean) -> MultiSet a -> MultiSet a
filter' p = M.filterWithKey (\k _ -> p k)

map' :: forall a b. Ord b => (a -> b) -> MultiSet a -> MultiSet b
map' f = foldrWithIndex (\k _ acc -> insert (f k) acc) M.empty

foldr' :: forall a b. (a -> b -> b) -> b -> MultiSet a -> b
foldr' f z = foldrWithIndex repF z
  where 
    repF a (Additive 1) b = f a b
    repF a (Additive n) b = repF a (Additive (n - 1)) (f a b)

-- small example

frequencies :: Array String
frequencies =
  split (Pattern " ") "I have so many words; words I so like to have words for!?"
  # map (toCharArray >>> A.filter isLetter >>> fromCharArray)
  # fromFoldable
  # M.toUnfoldable
  # A.sortWith (Down <<< snd)
  # map (\(Tuple w (Additive i)) -> w <> " => " <> show i)