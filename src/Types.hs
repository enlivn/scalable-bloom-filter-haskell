{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
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
                            mutBitsPerSlice :: Word32,
                            mutHashFns :: a -> [Word32],
                            mutBitArray :: STUArray s Word32 Bool
                        }

data ImmutableBloom a = ImmutableBloom {
                            immutBitsPerSlice :: Word32,
                            immutHashFns :: a -> [Word32],
                            immutBitArray :: UArray Word32 Bool -- note: no need for the ST monad since this is immutable
                        }
