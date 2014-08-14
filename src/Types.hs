{-|
Module      : Types
Description : Define types.
-}
module Types where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)

data MutableBloom s a = MutableBloom {
                            mutCap :: Word32, -- ^ capacity of filter
                            mutBitsPerSlice :: Word32, -- ^ number of bits per slice
                            mutHashFns :: a -> [Word32], -- ^ hash functions
                            mutBitArray :: STUArray s Word32 Bool -- ^ array storing the state of the filter
                        }

data ImmutableBloom a = ImmutableBloom {
                            immutCap :: Word32, -- ^ capacity of filter
                            immutBitsPerSlice :: Word32, -- ^ number of bits per slice
                            immutHashFns :: a -> [Word32], -- ^ hash functions
                            immutBitArray :: UArray Word32 Bool -- ^ array storing the state of the filter.
                                                                --   Note that we don't need the ST monad since this is immutable
                        }
