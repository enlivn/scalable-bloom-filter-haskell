{-# LANGUAGE ForeignFunctionInterface #-}
module Hash.Hash where

import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)
import Data.Word (Word32, Word64)
import Data.Bits ((.&.))

foreign import ccall unsafe "lookup3.h hashword2" hashWord2 :: Ptr Word32 -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "lookup3.h hashlittle2" hashLittle2 :: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

hash2 :: Word64 -> -- seed
         CSize  -> -- number of bytes to hash
         Ptr a  -> -- value to hash
         IO Word64
hash2 seed len val = with seed $ (\seedPtr -> do
    let ptrSeedLower = castPtr seedPtr
        ptrSeedUpper = plusPtr ptrSeedLower 4

        calcHash :: IO ()
        calcHash = if seed .&. 3 == 0 then -- 32-bit aligned
                    hashWord2 (castPtr val) (div len 4) ptrSeedLower ptrSeedUpper
                   else -- not 32-bit aligned
                    hashLittle2 val len ptrSeedLower ptrSeedUpper
    calcHash >> peek seedPtr)

