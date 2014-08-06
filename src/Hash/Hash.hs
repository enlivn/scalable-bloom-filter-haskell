{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances, ForeignFunctionInterface #-}
module Hash.Hash where

import Control.Monad (foldM)
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Bits ((.&.), shiftR)
import Data.ByteString as S (useAsCStringLen, ByteString, append, concat)
import Data.ByteString.Lazy as L (ByteString, null, splitAt, toChunks)
import Data.Word (Word32, Word64)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (sizeOf, Storable(..), peek, sizeOf, Storable(..))
import System.IO.Unsafe (unsafePerformIO)

class Hashable a where
    hashWithSeed :: Word64 -> a -> Word64

-- Bool, Char, Double, Float, Int*, Word* etc
-- need UndecidableInstances
-- need FlexibleInstances to add context Storable a
hashStorableWithSeed :: Storable a => Word64 -> a -> Word64
hashStorableWithSeed seed val = unsafePerformIO $ with val $ \valPtr -> do
    hash64 seed (fromIntegral $ sizeOf val) valPtr

instance Storable a => Hashable a where
    hashWithSeed = hashStorableWithSeed

-- lists
instance Storable a => Hashable [a] where
    hashWithSeed = hashStorableListWithSeed

hashStorableListWithSeed :: Storable a => Word64 -> [a] -> Word64
hashStorableListWithSeed seed list = unsafePerformIO $ withArrayLen list $ \listLen listPtr -> do
    hash64 seed (fromIntegral $ listLen * sizeOf (head list)) listPtr

-- tuples
instance (Hashable a, Hashable b) => Hashable (a, b) where
    hashWithSeed seed (x,y) = hash' x . hash' y $ seed
        where hash' :: Hashable a => a -> Word64 -> Word64
              hash' x seed = hashWithSeed seed x

-- strict bytestrings
-- bytestrings are much more efficient than strings (which are linked lists
-- of chars that themselves are 21-bit unicode codepoints)
instance Hashable S.ByteString where
    hashWithSeed seed = unsafePerformIO . hashStorableByteStringWithSeed seed

hashStorableByteStringWithSeed :: Word64 -> S.ByteString -> IO Word64
hashStorableByteStringWithSeed seed bs = S.useAsCStringLen bs $ \(strPtr, strLen) -> do
    hash64 seed (fromIntegral strLen) strPtr

-- lazy bytestrings
instance Hashable L.ByteString where
    hashWithSeed seed lbs = unsafePerformIO $ foldM hashStorableByteStringWithSeed seed (rechunked lbs)
        where
              rechunked :: L.ByteString -> [S.ByteString]
              rechunked l | L.null l = []
                          | otherwise = do
                            let (prefix,suffix) = L.splitAt (64 * k) l
                            makeStrict prefix : rechunked suffix

              makeStrict :: L.ByteString -> S.ByteString
              makeStrict = S.concat . L.toChunks

              k = 1024

foreign import ccall unsafe "lookup3.h hashword2"
    hashWord2 :: Ptr Word32 -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "lookup3.h hashlittle2"
    hashLittle2 :: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

-- | Generates multiple hash functions
-- The first argument is the number of hash functions to generate
-- The second argument is the value to hash
genHashes :: Hashable a => Int -> a -> [Word32]
genHashes numHashes val = [h2 + h1*i | i <- [0..(fromIntegral numHashes)]]
    where h = hashWithSeed 0x106fc397cf62f64d3 val
          h1 = fromIntegral h
          h2 = fromIntegral (shiftR h 32) .&. maxBound

-- | Generates a 64-bit hash at one go.
hash64 :: Storable a => Word64 -> CSize  -> Ptr a  -> IO Word64
hash64 seed len val = with seed $ (\seedPtr -> do
    let ptrSeedLower = castPtr seedPtr
        ptrSeedUpper = plusPtr ptrSeedLower 4

        calcHash :: IO ()
        calcHash = if seed .&. 3 == 0 then -- 32-bit aligned
                    hashWord2 (castPtr val) (div len 4) ptrSeedLower ptrSeedUpper
                   else -- not 32-bit aligned
                    hashLittle2 val len ptrSeedLower ptrSeedUpper
    calcHash >> peek seedPtr)
