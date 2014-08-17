{-|
Module      : ScalableBloomFilter
Description : Scalable bloom filter allowing insertion of elements
              post-construction.

This bloom filter follows the paper 'Scalable Bloom Filters' by Almeida et. al
(http://gsd.di.uminho.pt/members/cbm/ps/dbloom.pdf). It has a list of MutableBloom
bloom filters with progressively tighter maximum error probabilities so that
the compounded error probability of the scalable filter converges to the value
specified in the new function.
-}

module ScalableBloomFilter(new,
                           fromList,
                           size,
                           insert,
                           insertList,
                           elem,
                           notElem) where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Data.Word (Word32)
import Hash.Hash
import qualified MutableBloomFilter as M (new, size, isFull, insert, elem)
import Prelude hiding (elem, notElem)
import Types

-- | Create a scalable bloom filter.
-- The first argument is the compounded false positive rate desired (between 0 and 1) (P)
-- We start off with an initial capacity of 1024 elements
new :: Hashable a => Double -> ST s (ScalableBloom s a)
new p = do
    mb <- M.new p0 initCap -- initial capacity of 1024 elements
    initArray <- newSTRef [mb]
    return $ ScalableBloom initCap p initArray
    where p0 = p * (1-r) -- P converges to <= P0/(1-r). Thus, P0 = P*(1-r)
          r = 0.9
          initCap = 1024

-- | Create a scalable bloom filter and initialize it with the list passed in.
-- The first argument is the false positive rate desired (between 0 and 1) (P)
-- The second argument is the list of values with which to initialize the filter.
fromList :: Hashable a => Double -> [a] -> ST s (ScalableBloom s a)
fromList p initList = do
    sbf <- new p
    insertList sbf initList
    return sbf

-- | Returns the total size (M = m*k) in bits of the filter.
-- This is equal to the individual sizes of the constituent
-- filters.
size :: ScalableBloom s a -> ST s Word32
size filt = do
    constituentFilters <- readSTRef (filtArray filt)
    fmap sum $ mapM M.size constituentFilters

-- | Inserts an element into the filter.
-- The first argument is the filter
-- The second argument is the element to insert
insert :: Hashable a => ScalableBloom s a -> a -> ST s ()
insert filt element = do
    constituentFilters <- readSTRef (filtArray filt)
    let lastMb = last constituentFilters
        p = (mutErrRate lastMb) * r -- Pi = Pi-1 * r
        c = (mutCap lastMb) * s     -- Mi = Mi-1 * s
        r = 0.9
        s = 2
    lastFilterIsFull <- M.isFull lastMb
    if lastFilterIsFull then do
        -- create a new bloom filter, add the element to it and add the filter itself
        -- to the array of constituent filters in the scalable filter
        newFilter <- M.new p c
        M.insert newFilter element
        modifySTRef' (filtArray filt) (++ [newFilter])
    else M.insert lastMb element

-- | Inserts multiple elements into the filter.
-- The first argument is the filter
-- The second argument is the list of elements to insert
insertList :: Hashable a => ScalableBloom s a -> [a] -> ST s ()
insertList filt = mapM_ (insert filt)

-- | Returns True if the element is in the filter
-- There is a small chance that True will be returned even if the element
-- is NOT in the filter
elem :: ScalableBloom s a -> a -> ST s Bool
elem filt element = do
    constituentFilters <- liftM reverse $ readSTRef $ filtArray filt -- reverse is so we start looking in the filters
                                                                    -- that were most recently added
    find element constituentFilters
    where find :: a -> [MutableBloom s a] -> ST s Bool
          find _ [] = return False
          find e (x:xs) = M.elem x e >>= \found ->
            if found then return True
            else find e xs

-- | Complement of 'elem'.
notElem :: ScalableBloom s a -> a -> ST s Bool
notElem filt element = liftM not $ elem filt element
