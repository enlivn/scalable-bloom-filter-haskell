module Types where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)

-- unboxed/primitive types are always unlifted (and hence cannot be bottom)
-- unboxed/primitive types generally are raw values (i.e., they don't involve
-- any pointers or heap allocation; Array# is an exception because it is too
-- big to fit into a register)
-- you cannot pass a primitive type to a polymorphic function or store it in a
-- polymorphic data type (e.g. [Int#]) because the GC will attempt to follow
-- any polymorphic constructors or arguments under the assumption that it is a
-- pointer
-- boxed types are represented by a pointer to a heap object

data MutableBloom s a = MutableBloom {
                            mutBitsPerSlice :: Word32, -- ^ number of bits per slice
                            mutHashFns :: a -> [Word32], -- ^ hash functions
                            mutBitArray :: STUArray s Word32 Bool -- ^ array storing the state of the filter
                        }

data ImmutableBloom a = ImmutableBloom {
                            immutBitsPerSlice :: Word32, -- ^ number of bits per slice
                            immutHashFns :: a -> [Word32], -- ^ hash functions
                            immutBitArray :: UArray Word32 Bool -- ^ array storing the state of the filter.
                                                                --   Note that we don't need the ST monad since this is immutable
                        }
