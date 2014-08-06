{-|
Module      : MutableBloomFilter
Description : Mutable bloom filter allowing insertion of elements.

This bloom filter has a bit array that is partitioned into slices,
in line with the paper 'Scalable Bloom Filters' by Almeida et. al
(http://gsd.di.uminho.pt/members/cbm/ps/dbloom.pdf)
The main difference between this and the ImmutableBloomFilter is
that the latter does NOT allow insertion of elements once it has
been initialized.
-}
module MutableBloomFilter(MutableBloom,
                          new,
                          length,
                          insert,
                          elem,
                          notElem) where

import Control.Monad (liftM)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, getBounds, writeArray, readArray)
import Data.Array.ST (runSTUArray)
import Data.List (genericLength)
import Data.Word (Word32)
import Hash.Hash
import MutableBloomFilter.Internal as I
import Prelude hiding (length, elem, notElem)
import Types

-- | Create a mutable bloom filter.
-- The first argument is the false positive rate desired (between 0 and 1) (P)
-- The second argument is a list of values with which to initialize the filter
new :: Hashable a => Double -> Word32 -> Either String (ST s (MutableBloom s a))
new p n = case (I.calculateFilterParams p n) of
    Left err -> Left err
    Right (bitsPerSlice, numSlices) -> Right $ I.new' numSlices bitsPerSlice

-- | Returns the total length (M = m*k) of the filter.
length :: MutableBloom s a -> ST s Word32
length filt = fmap ((1 +) . snd) (getBounds (mutBitArray filt))

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
