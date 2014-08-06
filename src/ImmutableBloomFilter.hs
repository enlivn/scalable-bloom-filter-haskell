{-|
Module      : ImmutableBloomFilter
Description : Immutable bloom filter that, once initialized, does not
              allow insertion of any more elements.

The main difference between this and MutableBloomFilter is
that the latter allows insertion of elements even after it has
been created.
-}
module ImmutableBloomFilter (ImmutableBloom,
                             new,
                             length,
                             elem,
                             notElem) where

import Control.Monad (liftM)
import Control.Monad.ST (runST, ST)
import Types
import Hash.Hash
import Data.Array.Unboxed (bounds, (!))
import Data.Array.ST (STUArray, runSTUArray)
import Data.List (genericLength)
import Data.Word (Word32)
import MutableBloomFilter as M (insert)
import MutableBloomFilter.Internal as I
import Prelude hiding (length, elem, notElem)

-- | Create an immutable bloom filter.
-- The first argument is the false positive rate desired (between 0 and 1) (P)
-- The second argument is a list of values with which to initialize the filter
new :: (Hashable a) => Double -> [a] -> ImmutableBloom a
new falsePositiveRate initList = uncurry (ImmutableBloomFilter.new' initList) $
                                 I.calculateFilterParams falsePositiveRate (genericLength initList)

-- | Create an immutable bloom filter.
-- The first argument is the number of slices in the filter (k)
-- The second argument is the number of bits per slice (m)
-- The third argument is a list of values with which to initialize the filter
new' :: (Hashable a) => [a] -> Int -> Word32 -> ImmutableBloom a
new' initList numSlices bitsPerSlice = ImmutableBloom bitsPerSlice freezeHashFnsFromMutableBloom freezeArrayFromMutableBloom
    where freezeHashFnsFromMutableBloom = runST $ fmap mutHashFns mutableBloom
          freezeArrayFromMutableBloom = runSTUArray (fmap mutBitArray mutableBloom)
          mutableBloom = do
                           mutableBloom <- I.new' numSlices bitsPerSlice
                           mapM_ (M.insert mutableBloom) initList
                           return mutableBloom

-- | Returns the total length (M = m*k) of the filter.
length :: ImmutableBloom a -> Word32
length filt = ((1 +) . snd) (bounds (immutBitArray filt))

-- | Given an element to insert, return the corresponding indices that need
-- to be set in the filter
getHashIndices :: ImmutableBloom a -> a -> [Word32]
getHashIndices filt y = map (`mod` len) $ immutHashFns filt y
    where len = ImmutableBloomFilter.length filt

-- | Returns True if the element is in the filter
elem :: ImmutableBloom a -> a -> Bool
elem filt element = all (immutBitArray filt !) (getHashIndices filt element)

-- | Complement of 'elem'.
notElem :: ImmutableBloom a -> a -> Bool
notElem filt = not . elem filt
