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
new falsePositiveRate initList = case (calculateFilterParams falsePositiveRate (genericLength initList)) of
    Left err -> Left err
    Right (bitsPerSlice, numSlices) -> Right $ new' numSlices bitsPerSlice initList

-- | Given error rate (P) and capacity (n), returns (bits per slice (m), number of slices (k))
-- Note that m*k = M (filter size)
-- We assume in the equations below that the fill ratio (p) is optimal i.e., p = 1/2. 'Optimal' here means
-- we've maximized the capacity n
calculateFilterParams :: Double -> Word32 -> Either String (Word32, Int)
calculateFilterParams p n | n <= 0 = Left "n must be strictly positive"
                        | p <= 0 || p >= 1 = Left "p must be between 0 and 1"
                        | otherwise = Right (fromIntegral . truncate $ m, k)
    where k = ceiling $ logBase 2 (1 / p) :: Int-- k = log2 (1/P)
          m = n' * abs (log p) / (k' * (log 2)**2) :: Double -- m = n * abs(log(P))/(k*(ln2)^2)
          n' = fromIntegral n :: Double
          k' = fromIntegral k :: Double

-- | Create an immutable bloom filter.
-- The first argument is the number of slices in the filter (k)
-- The second argument is the number of bits per slice (m)
-- The third argument is a list of values with which to initialize the filter
new' :: (Hashable a) => Int -> Word32 -> [a] -> ImmutableBloom a
new' numSlices bitsPerSlice initList = ImmutableBloom bitsPerSlice freezeHashFnsFromMutableBloom freezeArrayFromMutableBloom
    where freezeHashFnsFromMutableBloom = runST $ fmap mutHashFns mutableBloom
          freezeArrayFromMutableBloom = runSTUArray (fmap mutBitArray mutableBloom)
          mutableBloom = do
                           mutableBloom <- M.new numSlices bitsPerSlice
                           mapM_ (M.insert mutableBloom) initList
                           return mutableBloom

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
