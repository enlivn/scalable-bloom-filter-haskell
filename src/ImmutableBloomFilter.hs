module ImmutableBloomFilter (ImmutableBloom,
                             ImmutableBloomFilter.new,
                             length,
                             elem,
                             notElem) where

import Control.Monad (liftM)
import Control.Monad.ST (runST)
import Types
import Hash.Hash
import Data.Array.Unboxed (bounds, (!))
import Data.Array.ST (runSTUArray)
import Data.List (genericLength)
import Data.Word (Word32)
import Prelude hiding (length, elem, notElem)
import MutableBloomFilter as M (new, insert)

-- | Create an immutable bloom filter.
-- The first argument is the false positive rate desired (between 0 and 1) (P)
-- The second argument is a list of values with which to initialize the filter
new :: (Hashable a) => Double -> [a] -> Either String (ImmutableBloom a)
new falsePositiveRate initList = case (calculateFilterSize falsePositiveRate (genericLength initList)) of
    Left err -> Left err
    Right (bitsPerSlice, numSlices) -> Right $ new' numSlices bitsPerSlice initList
    where calculateFilterSize :: Double -> Word32 -> Either String (Word32, Int)
          calculateFilterSize = undefined

-- | Create an immutable bloom filter.
-- The first argument is the number of slices in the filter (k)
-- The second argument is the number of bits per slice (m)
-- The third argument is a list of values with which to initialize the filter
new' :: (Hashable a) => Int -> Word32 -> [a] -> ImmutableBloom a
new' numSlices bitsPerSlice initList = ImmutableBloom bitsPerSlice freezeHashFnsFromMutableBloom freezeArrayFromMutableBloom
    where freezeArrayFromMutableBloom = runSTUArray (fmap mutBitArray mutableBloom)
          mutableBloom = do
                           mutableBloom <- M.new numSlices bitsPerSlice
                           mapM_ (M.insert mutableBloom) initList
                           return mutableBloom
          freezeHashFnsFromMutableBloom = runST $ fmap mutHashFns mutableBloom

-- | Returns the total length (M = m*k) of the filter.
length :: ImmutableBloom a -> Word32
length filt = ((1 +) . snd) (bounds (immutBitArray filt))

-- | Given an element to insert, return the corresponding indices that need
-- to be set in the filter
getHashIndices :: ImmutableBloom a -> a -> [Word32]
getHashIndices filt y = map (`mod` len) $ immutHashFns filt y
    where len = ImmutableBloomFilter.length filt

-- | Returns True if the element is in the filter
elem :: ImmutableBloom a -> a -> Bool
elem filt element = all (immutBitArray filt !) (getHashIndices filt element)

-- | Complement of 'elem'.
notElem :: ImmutableBloom a -> a -> Bool
notElem filt = not . elem filt
