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
-- The first argument is the number of slices in the filter
-- The second argument is the number of bits in each slice
new :: (Hashable a) => Int -> Word32 -> ST s (MutableBloom s a)
new numSlices bitsPerSlice = return . MutableBloom bitsPerSlice (genHashes numSlices) =<< newArray (0, fromIntegral numSlices * bitsPerSlice) False

length :: MutableBloom s a -> ST s Word32
length x = fmap ((1 +) . snd) (getBounds (mutBitArray x))

insert :: MutableBloom s a -> a -> ST s ()
insert x y = getHashIndices x y >>= mapM_ (\bit -> writeArray (mutBitArray x) bit True)

-- | Given an element to insert, return the corresponding indices that need
-- to be set in the filter
getHashIndices :: MutableBloom s a -> a -> ST s [Word32]
getHashIndices x y = length x >>= \len -> return . addSliceOffsets $ perSliceIndices
    where bitsPerSlice = mutBitsPerSlice x
          addSliceOffsets = (flip . zipWith) (\x offset -> offset*bitsPerSlice + x) [0..]
          perSliceIndices = map (`mod` bitsPerSlice) $ mutHashFns x y

elem :: MutableBloom s a -> a -> ST s Bool
elem x y = liftM and $ getHashIndices x y >>= mapM (readArray (mutBitArray x))

notElem :: MutableBloom s a -> a -> ST s Bool
notElem x y = liftM not $ elem x y
