{-|
Module      : MutableBloomFilter
Description : Mutable bloom filter allowing insertion of elements.

This bloom filter has a bit array that is partitioned into slices,
in line with the paper 'Scalable Bloom Filters' by Almeida et. al
(http://gsd.di.uminho.pt/members/cbm/ps/dbloom.pdf)
The main differences between this and ImmutableBloomFilter are:
    1. the latter can be used from pure code
    2. the latter does NOT allow insertion of elements once initialized.
-}
{-# LANGUAGE RankNTypes #-}

module MutableBloomFilter(MutableBloom,
                          new,
                          length,
                          insert,
                          elem,
                          notElem) where

import Control.Monad (liftM)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (MArray, newArray, getBounds, writeArray, readArray, freeze)
import Data.Array.ST (runSTUArray, STUArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.List (genericLength)
import Data.Word (Word32)
import Data.Array.Unboxed (UArray)
import Hash.Hash
import Prelude hiding (length, elem, notElem)
import Types

-- | Create a mutable bloom filter.
-- The first argument is the false positive rate desired (between 0 and 1) (P)
-- The second argument is a list of values with which to initialize the filter
new :: Hashable a => Double -> Word32 -> ST s (MutableBloom s a)
new p n = uncurry new' (calculateFilterParams p n)

-- | Convert a mutable bloom filter in ST to an immutable bloom filter suitable
-- for access from pure code
toImmutable :: (forall s. ST s (MutableBloom s a)) -> ImmutableBloom a
toImmutable mb = runST $ do
    mutableBloom <- mb
    let bitsPerSlice = mutBitsPerSlice mutableBloom
        hashFuns = mutHashFns mutableBloom
    f <- unsafeFreeze $ mutBitArray mutableBloom -- for copying STUArray -> UArray, unsafeFreeze is
                                                 -- O(n) if compiled without -o,
                                                 -- O(1) if compiled with -o
    return $ ImmutableBloom bitsPerSlice hashFuns f

-- | Convert a mutable bloom filter to an immutable bloom filter in ST
toImmutable' :: Hashable a => (MutableBloom s a) -> ST s (ImmutableBloom a)
toImmutable' (MutableBloom bitsPerSlice hashFuns bitArray) = return . ImmutableBloom bitsPerSlice hashFuns =<< freeze bitArray

-- | Calculate the bits per slice (m) and number of slices (k) filter parameters
-- The first argument is the desired error rate (P)
-- The second argument is the capacity (n)
-- Returns (number of slices, bits per slice)
-- Note that m*k = M (filter size)
-- We assume in the equations below that the fill ratio (p) is optimal i.e., p = 1/2. 'Optimal' here means
-- we've maximized the capacity n
calculateFilterParams :: Double -> Word32 -> (Int, Word32)
calculateFilterParams p n | n <= 0 = error "n must be strictly positive"
                          | p <= 0 || p >= 1 = error "p must be between 0 and 1"
                          | otherwise = (k, fromIntegral . truncate $ m)
    where k = ceiling $ logBase 2 (1 / p) :: Int-- k = log2 (1/P)
          m = n' * abs (log p) / (k' * log 2 **2) :: Double -- m = n * abs(log(P))/(k*(ln2)^2)
          n' = fromIntegral n :: Double
          k' = fromIntegral k :: Double

-- | Create a mutable bloom filter
-- The first argument is the number of slices in the filter (k)
-- The second argument is the number of bits in each slice (m)
new' :: Hashable a => Int -> Word32 -> ST s (MutableBloom s a)
new' numSlices bitsPerSlice = return . MutableBloom bitsPerSlice (genHashes numSlices) =<< newArray (0, _M) False
    where _M = fromIntegral numSlices * bitsPerSlice -- ^ total number of bits in the filter (M = k * m)

-- | Returns the total length (M = m*k) of the filter.
length :: MutableBloom s a -> ST s Word32
length (MutableBloom _ _ bitArray) = fmap ((1 +) . snd) (getBounds bitArray)

-- | Inserts an element into the filter.
-- The first argument is the filter
-- The second argument is the element to insert
insert :: MutableBloom s a -> a -> ST s ()
insert filt element = getHashIndices filt element >>= mapM_ (\bit -> writeArray (mutBitArray filt) bit True)

-- | Given an element to insert, return the corresponding indices that need
-- to be set in the filter
getHashIndices :: MutableBloom s a -> a -> ST s [Word32]
getHashIndices filt element = return . addSliceOffsets $ indicesWithinSlice
    where addSliceOffsets = (flip . zipWith) (\indexWithinSlice sliceOffset -> sliceOffset*bitsPerSlice + indexWithinSlice) [0..]
          indicesWithinSlice = map (`mod` bitsPerSlice) $ mutHashFns filt element
          bitsPerSlice = mutBitsPerSlice filt

-- | Returns True if the element is in the filter
elem :: MutableBloom s a -> a -> ST s Bool
elem filt element = liftM and $ getHashIndices filt element >>= mapM (readArray (mutBitArray filt))

-- | Complement of 'elem'.
notElem :: MutableBloom s a -> a -> ST s Bool
notElem filt element = liftM not $ elem filt element
