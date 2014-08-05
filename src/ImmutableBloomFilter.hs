module ImmutableBloomFilter (ImmutableBloom,
                             new',
                             ImmutableBloomFilter.new,
                             length,
                             elem,
                             notElem) where

import Control.Monad (liftM)
import Control.Monad.ST (runST)
import Types
import Hash.Hash
import Data.Array.Unboxed (bounds, (!))
import Data.Array.ST (runSTUArray)
import Data.List (genericLength)
import Data.Word (Word32)
import Prelude hiding (length, elem, notElem)
import MutableBloomFilter as M (new, insert)

new :: (Hashable a) => Double -> [a] -> Either String (ImmutableBloom a)
new falsePositiveRate initList = case (calculateFilterSize falsePositiveRate (genericLength initList)) of
    Left err -> Left err
    Right (bitsPerSlice, numSlices) -> Right $ new' numSlices bitsPerSlice initList
    where calculateFilterSize :: Double -> Word32 -> Either String (Word32, Int)
          calculateFilterSize = undefined

-- create an immutable bloom filter.
-- in addition to the hash functions and the number of bits you also need
-- to provide a list of values with which to initialize the bloom filter
new' :: (Hashable a) => Int -> Word32 -> [a] -> ImmutableBloom a
new' numSlices bitsPerSlice initList = ImmutableBloom bitsPerSlice freezeHashFnsFromMutableBloom freezeArrayFromMutableBloom
    where freezeArrayFromMutableBloom = runSTUArray (fmap mutBitArray mutableBloom)
          mutableBloom = do
                           mutableBloom <- M.new numSlices bitsPerSlice
                           mapM_ (M.insert mutableBloom) initList
                           return mutableBloom
          freezeHashFnsFromMutableBloom = runST $ fmap mutHashFns mutableBloom

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
