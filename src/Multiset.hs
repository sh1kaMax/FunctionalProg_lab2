module Multiset
  ( createMultiset,
    Multiset.insert,
    Multiset.delete,
    Multiset.find,
    Multiset.filterMultiset,
    Multiset.mapMultiset,
    Multiset.foldlMultiset,
    Multiset.foldrMultiset,
  )
where

import Data.Hashable
import SCHashMap
  ( SCHashMap,
    Value (..),
    delete,
    emptyHashMap,
    filterHashMap,
    find,
    foldlHashMap,
    foldrHashMap,
    insert,
    mapHashMap,
  )

newtype Multiset a = Multiset (SCHashMap a)

instance Show a => Show (Multiset a) where
  show (Multiset m) = show m

instance (Ord a) => Semigroup (Multiset a) where
  (<>) (Multiset m1) (Multiset m2) = Multiset (m1 <> m2)

instance (Ord a) => Monoid (Multiset a) where
  mempty = createMultiset

createMultiset :: Multiset a
createMultiset = Multiset emptyHashMap

insert :: (Hashable a) => Multiset a -> a -> Multiset a
insert (Multiset multiset) v = Multiset (SCHashMap.insert multiset (Value v))

delete :: (Hashable a) => Multiset a -> a -> Multiset a
delete (Multiset multiset) v = Multiset (SCHashMap.delete multiset (Value v))

find :: (Hashable a) => Multiset a -> a -> Bool
find (Multiset multiset) v = SCHashMap.find multiset (Value v)

filterMultiset :: (a -> Bool) -> Multiset a -> Multiset a
filterMultiset func (Multiset multiset) = Multiset (filterHashMap func multiset)

mapMultiset :: (a -> b) -> Multiset a -> Multiset b
mapMultiset func (Multiset multiset) = Multiset (mapHashMap func multiset)

foldlMultiset :: (b -> a -> b) -> b -> Multiset a -> b
foldlMultiset func acc (Multiset multiset) = foldlHashMap func acc multiset

foldrMultiset :: (a -> b -> b) -> b -> Multiset a -> b
foldrMultiset func acc (Multiset multiset) = foldrHashMap func acc multiset
