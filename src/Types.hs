{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Types where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32, Word64)
import Hash.Hash
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Storable (sizeOf, Storable(..))
import System.IO.Unsafe (unsafePerformIO)

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
                            mutHashFns :: a -> [Word32],
                            mutBitArray :: STUArray s Word32 Bool
                        }

data ImmutableBloom a = ImmutableBloom {
                            immutHashFns :: a -> [Word32],
                            immutBitArray :: UArray Word32 Bool -- note: no need for the ST monad since this is immutable
                        }

hashStorableWithSeed :: Storable a => Word64 -> a -> Word64
hashStorableWithSeed seed val = unsafePerformIO $ with val $ \valPtr -> do
    hash2 seed (fromIntegral $ sizeOf val) valPtr

hashStorableListWithSeed :: Storable a => Word64 -> [a] -> Word64
hashStorableListWithSeed seed list = unsafePerformIO $ withArrayLen list $ \listLen listPtr -> do
    hash2 seed (fromIntegral $ listLen * sizeOf (head list)) listPtr

class Hashable a where
    hashWithSeed :: Word64 -> a -> Word64

-- covers Bool, Char, Double, Float, Int*, Word* etc
-- need UndecidableInstances
-- need FlexibleInstances to add context Storable a
instance Storable a => Hashable a where
    hashWithSeed = hashStorableWithSeed

-- covers list of Hashable Storable a's as defined in the instance above
instance Storable a => Hashable [a] where
    hashWithSeed = hashStorableListWithSeed
