module ImmutableBloomFilter (ImmutableBloom,
                             new',
                             ImmutableBloomFilter.new,
                             length,
                             elem,
                             notElem) where

import Types
import Hash.Hash
import Data.Array.Unboxed (bounds, (!))
import Data.Array.ST (runSTUArray)
import Data.Word (Word32)
import Prelude hiding (length, elem, notElem)
import MutableBloomFilter as M (new, insert)

new :: (Hashable a) => Double -> [a] -> Either String (ImmutableBloom a)
new = undefined

-- create an immutable bloom filter.
-- in addition to the hash functions and the number of bits you also need to provide a list of values with which to initialize the bloom filter
new' :: (a -> [Word32]) -> Word32 -> [a] -> ImmutableBloom a
new' hashFns numBits initList = ImmutableBloom hashFns $ runSTUArray createAndInitMutableArray
    where createAndInitMutableArray = do
                                        mutableBloom <- M.new hashFns numBits
                                        mapM_ (M.insert mutableBloom) initList
                                        return $ mutBitArray mutableBloom

length :: ImmutableBloom a -> Word32
length x = ((1 +) . snd) (bounds (immutBitArray x))

-- given an element to insert, return the corresponding indices that need
-- to be set in the filter
getHashIndices :: ImmutableBloom a -> a -> [Word32]
getHashIndices x y = map (`mod` len) $ immutHashFns x y
    where len = ImmutableBloomFilter.length x

elem :: ImmutableBloom a -> a -> Bool
elem x y = all (immutBitArray x !) (getHashIndices x y)

notElem :: ImmutableBloom a -> a -> Bool
notElem x = not . ImmutableBloomFilter.elem x
