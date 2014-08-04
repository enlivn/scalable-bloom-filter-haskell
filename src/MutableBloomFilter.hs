module MutableBloomFilter( MutableBloom,
                           elem,
                           notElem,
                           insert,
                           length,
                           new) where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.Array.MArray (newArray, getBounds, writeArray, readArray)
import Data.Word (Word32)
import Prelude hiding (length, elem, notElem)
import Types

-- create a mutable bloom filter
new :: (a -> [Word32]) -> Word32 -> ST s (MutableBloom s a)
new hashFns numBits = return . MutableBloom hashFns =<< newArray (0, numBits) False

length :: MutableBloom s a -> ST s Word32
length x = fmap ((1 +) . snd) (getBounds (mutBitArray x))

insert :: MutableBloom s a -> a -> ST s ()
insert x y = getHashIndices x y >>= mapM_ (\bit -> writeArray (mutBitArray x) bit True)

-- given an element to insert, return the corresponding indices that need
-- to be set in the filter
getHashIndices :: MutableBloom s a -> a -> ST s [Word32]
getHashIndices x y = length x >>= \len -> return $ map (`mod` len) $ mutHashFns x y

elem :: MutableBloom s a -> a -> ST s Bool
elem x y = liftM and $ getHashIndices x y >>= mapM (readArray (mutBitArray x))

notElem :: MutableBloom s a -> a -> ST s Bool
notElem x y = liftM not $ elem x y
