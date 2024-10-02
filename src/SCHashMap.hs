module SCHashMap
  ( SCHashMap,
    show,
    emptyHashMap,
    insert,
    delete,
    filterHashMap,
    mapHashMap,
    foldlHashMap,
    foldrHashMap,
    find,
    Value (..),
  )
where

import Data.Hashable (Hashable, hash)

type Key = Int

type Count = Int

newtype Value a = Value a deriving (Eq, Ord)

newtype Bucket a = Bucket [(Value a, Count)]

newtype SCHashMap a = SCHashMap [(Key, Bucket a)]

instance (Show a) => Show (Value a) where
  show (Value v) = show v

instance (Show a) => Show (Bucket a) where
  show (Bucket xs) = bucket
    where
      bucket =
        if null xs
          then ""
          else init (init (concatMap showConcat xs))
      showConcat (v, c) = concat (replicate c (show v ++ ", "))

instance (Show a) => Show (SCHashMap a) where
  show (SCHashMap xs) = "[" ++ notNullMap ++ "]"
    where
      myMap = concatMap showMapConcat xs
      notNullMap
        | null myMap = ""
        | otherwise = init (init myMap)
      showMapConcat (_, b)
        | null (show b) = ""
        | otherwise = show b ++ ", "

instance (Ord a) => Semigroup (SCHashMap a) where
  (<>) (SCHashMap hashmap1) (SCHashMap hashmap2) = SCHashMap newHashMap
    where
      newHashMap = [(k1, combineBucket b1 b2) | (k1, b1) <- hashmap1, (k2, b2) <- hashmap2, k1 == k2]

      combineBucket :: (Ord a) => Bucket a -> Bucket a -> Bucket a
      combineBucket (Bucket bucket1) (Bucket bucket2) =
        let mergeBuckets [] b = b
            mergeBuckets a [] = a
            mergeBuckets a@(x : xs) b@(y : ys)
              | fst x == fst y = (fst x, snd x + snd y) : mergeBuckets xs ys
              | fst x < fst y = x : mergeBuckets xs b
              | otherwise = y : mergeBuckets a xs
         in Bucket (mergeBuckets bucket1 bucket2)

instance (Ord a) => Monoid (SCHashMap a) where
  mempty = emptyHashMap

bucketsCount :: Int
bucketsCount = 2 ^ (4 :: Integer)

emptyHashMap :: SCHashMap a
emptyHashMap = SCHashMap [(key, Bucket []) | key <- [0 .. bucketsCount - 1]]

hashFunc :: (Hashable a) => Value a -> Key
hashFunc (Value val) = hash val `mod` bucketsCount

insert :: (Hashable a) => SCHashMap a -> Value a -> SCHashMap a
insert (SCHashMap hashMap) value = SCHashMap newHashMap
  where
    key = hashFunc value

    (Bucket bucket) = snd (hashMap !! key)

    toUpdateValue = filter (\pair -> fst pair == value) bucket

    newBucket =
      if length toUpdateValue == 1
        then filter (\pair -> fst pair /= value) bucket ++ [(value, snd (head toUpdateValue) + 1)]
        else bucket ++ [(value, 1)]

    newHashMap = take key hashMap ++ [(key, Bucket newBucket)] ++ drop (key + 1) hashMap

delete :: (Hashable a) => SCHashMap a -> Value a -> SCHashMap a
delete (SCHashMap hashMap) value = SCHashMap newHashMap
  where
    key = hashFunc value

    (Bucket bucket) = snd (hashMap !! key)

    toUpdateValue = filter (\pair -> fst pair == value) bucket

    newBucket
      | null toUpdateValue = error "Error: value doesn't exist"
      | snd (head toUpdateValue) == 1 = filter (\pair -> fst pair /= value) bucket
      | otherwise = filter (\pair -> fst pair /= value) bucket ++ [(value, snd (head toUpdateValue) - 1)]

    newHashMap = take key hashMap ++ [(key, Bucket newBucket)] ++ drop (key + 1) hashMap

find :: (Hashable a) => SCHashMap a -> Value a -> Bool
find (SCHashMap hashMap) value = not (null findValue)
  where
    key = hashFunc value
    (Bucket bucket) = snd (hashMap !! key)

    findValue = filter (\pair -> fst pair == value) bucket

filterHashMap :: (a -> Bool) -> SCHashMap a -> SCHashMap a
filterHashMap func (SCHashMap hashMap) = SCHashMap newHashMap
  where
    newHashMap = [(key, Bucket filteredBucket) | (key, Bucket bucket) <- hashMap, let filteredBucket = filter (\(Value v, _) -> func v) bucket]

mapHashMap :: (a -> b) -> SCHashMap a -> SCHashMap b
mapHashMap func (SCHashMap hashMap) = SCHashMap newHashMap
  where
    newHashMap = [(key, Bucket mapedBucket) | (key, Bucket bucket) <- hashMap, let mapedBucket = map (\(Value v, count) -> (Value (func v), count)) bucket]

foldlHashMap :: (b -> a -> b) -> b -> SCHashMap a -> b
foldlHashMap func acc (SCHashMap hashMap) = foldl bucketFold acc hashMap
  where
    bucketFold acc' (_, Bucket bucket) = foldl valueFold acc' bucket
    valueFold acc'' (Value v, count) = iterate (`func` v) acc'' !! count

foldrHashMap :: (a -> b -> b) -> b -> SCHashMap a -> b
foldrHashMap func acc (SCHashMap hashmap) = foldr bucketFold acc hashmap
  where
    bucketFold (_, Bucket bucket) acc' = foldr valueFold acc' bucket
    valueFold (Value v, count) acc'' = foldr (\_ acc''' -> func v acc''') acc'' [1 .. count]
