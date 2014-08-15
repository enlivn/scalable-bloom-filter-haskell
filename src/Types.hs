{-|
Module      : Types
Description : Define types.
-}
module Types where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.STRef (STRef)
import Data.Word (Word32)

data MutableBloom s a = MutableBloom {
                            mutCap :: Word32,                     -- ^ capacity of filter (n)
                            mutCurCount :: STRef s Word32,        -- ^ num of elements inserted into the filter at a given time
                            mutBitsPerSlice :: Word32,            -- ^ number of bits per slice (m)
                            mutHashFns :: a -> [Word32],          -- ^ hash functions
                            mutBitArray :: STUArray s Word32 Bool -- ^ array storing the state of the filter
                        }

data ImmutableBloom a = ImmutableBloom {
                            immutCap :: Word32,                   -- ^ capacity of filter (n)
                            immutCurCount :: Word32,              -- ^ num of elements inserted into the filter at a given time
                            immutBitsPerSlice :: Word32,          -- ^ number of bits per slice (m)
                            immutHashFns :: a -> [Word32],        -- ^ hash functions
                            immutBitArray :: UArray Word32 Bool   -- ^ array storing the state of the filter.
                                                                  --   Note that we don't need the ST monad since this is immutable
                        }

data ScalableBloom s a = ScalableBloom {
                            cap :: Word32,                        -- ^ capacity of scalable filter
                            errRate :: Double,                    -- ^ compounded false positive rate (P) of scalable filter
                            filtArray :: [MutableBloom s a]       -- ^ array of constituent bloom filters
                         }
