{-# LANGUAGE OverlappingInstances #-}
module MutableBloomFilter( MutableBloom,
                           elem,
                           notElem,
                           insert,
                           length,
                           new) where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.Array.MArray (newArray, getBounds, writeArray, readArray)
import Data.List (genericLength)
import Data.Word (Word32)
import Hash.Hash
import Prelude hiding (length, elem, notElem)
import Types

-- | The 'new' function creates a mutable bloom filter
-- The first argument is the number of slices in the filter (k)
-- The second argument is the number of bits in each slice (m)
new :: (Hashable a) => Int -> Word32 -> ST s (MutableBloom s a)
new numSlices bitsPerSlice = return . MutableBloom bitsPerSlice (genHashes numSlices) =<< newArray (0, _M) False
    where _M = fromIntegral numSlices * bitsPerSlice -- ^ total number of bits in the filter (M = k * m)

length :: MutableBloom s a -> ST s Word32
length filt = fmap ((1 +) . snd) (getBounds (mutBitArray filt))

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
