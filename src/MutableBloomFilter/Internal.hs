{-|
Module      : MutableBloomFilter.Internal
Description : Module for internal use only.

Provides convenience functions pertaining to mutable bloom filters, for
use in both the MutableBloomFilter and ImmutableBloomFilter modules.
-}
module MutableBloomFilter.Internal where

import Types
import Hash.Hash
import Data.Word (Word32)
import Control.Monad.ST (ST)
import Data.Array.MArray (newArray)

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
          m = n' * abs (log p) / (k' * (log 2)**2) :: Double -- m = n * abs(log(P))/(k*(ln2)^2)
          n' = fromIntegral n :: Double
          k' = fromIntegral k :: Double

-- | Create a mutable bloom filter
-- The first argument is the number of slices in the filter (k)
-- The second argument is the number of bits in each slice (m)
new' :: Hashable a => Int -> Word32 -> ST s (MutableBloom s a)
new' numSlices bitsPerSlice = return . MutableBloom bitsPerSlice (genHashes numSlices) =<< newArray (0, _M) False
    where _M = fromIntegral numSlices * bitsPerSlice -- ^ total number of bits in the filter (M = k * m)

