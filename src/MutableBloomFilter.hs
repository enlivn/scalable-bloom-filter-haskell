{-|
Module      : MutableBloomFilter
Description : Mutable bloom filter allowing insertion of elements
              post-construction.

This bloom filter has a bit array that is partitioned into slices,
in line with the paper 'Scalable Bloom Filters' by Almeida et. al
(http://gsd.di.uminho.pt/members/cbm/ps/dbloom.pdf)
The main differences between this and ImmutableBloomFilter are:
    1. the latter can be used from pure code
    2. the latter does NOT allow insertion of elements once initialized.
-}

{-# LANGUAGE RankNTypes #-}

module MutableBloomFilter(MutableBloom,
                          ImmutableBloom,
                          new,
                          fromList,
                          size,
                          getCount,
                          isFull,
                          insert,
                          insertList,
                          elem,
                          toImmutable,
                          notElem) where

import Control.Monad (liftM)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (MArray, newArray, getBounds, writeArray, readArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.List (genericLength)
import Data.STRef (newSTRef, readSTRef, modifySTRef)
import Data.Word (Word32)
import Hash.Hash
import Prelude hiding (elem, notElem)
import Types

-- | Create a mutable bloom filter.
-- The first argument is the false positive rate desired (between 0 and 1) (P)
-- The second argument is the capacity of the filter (n)
new :: Hashable a => Double -> Word32 -> ST s (MutableBloom s a)
new p n = uncurry (new' p n) (calculateFilterParams p n)

-- | Create a mutable bloom filter and initialize it with the list passed in.
-- The first argument is the false positive rate desired (between 0 and 1) (P)
-- The second argument is the list of values with which to initialize the filter.
fromList :: Hashable a => Double -> [a] -> ST s (MutableBloom s a)
fromList p initList = new p (genericLength initList) >>= \mb -> do
    insertList mb initList
    return mb

-- | Convert a mutable bloom filter in ST to an immutable bloom filter suitable
-- for accessing from pure code
toImmutable :: (forall s. ST s (MutableBloom s a)) -> ImmutableBloom a
toImmutable mb = runST $ do
    mutableBloom <- mb
    let errRate = mutErrRate mutableBloom
        capacity = mutCap mutableBloom
        bitsPerSlice = mutBitsPerSlice mutableBloom
        hashFuns = mutHashFns mutableBloom
    curCount <- runST $ return $ getCount mutableBloom
    f <- unsafeFreeze $ mutBitArray mutableBloom -- for copying STUArray -> UArray, unsafeFreeze is
                                                 -- O(n) if compiled without -o,
                                                 -- O(1) if compiled with -o
    return $ ImmutableBloom errRate capacity curCount bitsPerSlice hashFuns f

-- | Returns the number of elements inserted in the filter
getCount :: MutableBloom s a -> ST s Word32
getCount = readSTRef . mutCurCount

-- | Returns True if the number of elements inserted in the filter
-- is equal to the capacity of the filter
-- Returns False otherwise
isFull :: MutableBloom s a -> ST s Bool
isFull filt@(MutableBloom _ cap _ _ _ _)= getCount filt >>= \curCount -> do
    if (curCount == cap) then
        return True
    else
        return False

-- | Convert a mutable bloom filter to an immutable bloom filter in ST
--toImmutable' :: Hashable a => (MutableBloom s a) -> ST s (ImmutableBloom a)
--toImmutable' (MutableBloom bitsPerSlice hashFuns bitArray) = return . ImmutableBloom bitsPerSlice hashFuns
--                                                             =<< unsafeFreeze bitArray -- for copying STUArray -> UArray, unsafeFreeze is
--                                                                                       -- O(n) if compiled without -o,
--                                                                                       -- O(1) if compiled with -o

-- | Calculate the bits per slice (m) and number of slices (k) filter parameters
-- The first argument is the desired error rate (P)
-- The second argument is the capacity (n)
-- Returns (number of slices k, bits per slice m)
-- Note that m*k = M (filter size)
-- We assume in the equations below that the fill ratio (p) is optimal i.e., p = 1/2. 'Optimal' here means
-- we've maximized the capacity n
calculateFilterParams :: Double -> Word32 -> (Int, Word32)
calculateFilterParams p n | n <= 0 = error "n must be strictly positive"
                          | p <= 0 || p >= 1 = error "p must be between 0 and 1"
                          | otherwise = (k, fromIntegral m)
    where k = ceiling $ logBase 2 (1 / p) :: Int -- k = log2 (1/P)
          m = ceiling $ n' * abs (log p) / (k' * log 2 **2) -- m = n * abs(log(P))/(k*(ln2)^2)
          n' = fromIntegral n :: Double
          k' = fromIntegral k :: Double

-- | Create a mutable bloom filter
-- The first argument is the number of slices in the filter (k)
-- The second argument is the number of bits in each slice (m)
new' :: Hashable a => Double -> Word32 -> Int -> Word32 -> ST s (MutableBloom s a)
new' p capacity numSlices bitsPerSlice = do
    initCount <- newSTRef 0
    initArray <- newArray (0, _M) False
    return $ MutableBloom p
                          capacity
                          initCount
                          bitsPerSlice
                          (genHashes numSlices)
                          initArray
    where _M = fromIntegral numSlices * bitsPerSlice -- ^ total number of bits in the filter (M = k * m)

-- | Returns the total size (M = m*k) in bits of the filter.
size :: MutableBloom s a -> ST s Word32
size filt = fmap ((1 +) . snd) (getBounds . mutBitArray $ filt)

-- | Inserts an element into the filter.
-- The first argument is the filter
-- The second argument is the element to insert
insert :: Hashable a => MutableBloom s a -> a -> ST s ()
insert filt element = do
    indicesToSet <- getHashIndices filt element
    present <- elem' filt element indicesToSet
    if present then
        return ()
    else do
        modifySTRef (mutCurCount filt) (1 +)
        mapM_ (\bit -> writeArray (mutBitArray filt) bit True) indicesToSet

-- | Inserts multiple elements into the filter.
-- The first argument is the filter
-- The second argument is the list of elements to insert
insertList :: Hashable a => MutableBloom s a -> [a] -> ST s ()
insertList filt = mapM_ (insert filt)

-- | Given an element to insert, return the corresponding indices that should
-- be set in the filter
getHashIndices :: MutableBloom s a -> a -> ST s [Word32]
getHashIndices filt element = return . addSliceOffsets $ indicesWithinSlice
    where addSliceOffsets = (flip . zipWith) (\indexWithinSlice sliceOffset ->
                                                sliceOffset*bitsPerSlice + indexWithinSlice)
                                             [0..]
          indicesWithinSlice = map (`mod` bitsPerSlice) $ mutHashFns filt element
          bitsPerSlice = mutBitsPerSlice filt

-- | Returns True if the element is in the filter
-- There is a small chance that True will be returned even if the element
-- is NOT in the filter
elem :: MutableBloom s a -> a -> ST s Bool
elem filt element = getHashIndices filt element >>= elem' filt element

elem' :: MutableBloom s a -> a -> [Word32] -> ST s Bool
elem' filt element indicesToCheck = liftM and (mapM (readArray (mutBitArray filt)) indicesToCheck)

-- | Complement of 'elem'.
notElem :: MutableBloom s a -> a -> ST s Bool
notElem filt element = liftM not $ elem filt element
