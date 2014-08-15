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

{-# LANGUAGE ScopedTypeVariables #-}

module ScalableBloomFilter(new,
                           size,
                           insert,
                           insertList,
                           elem,
                           notElem) where

import Control.Monad.ST (ST, runST)
import Data.Array.MArray (getElems, newArray, writeArray)
import Data.Word (Word32)
import Hash.Hash
import qualified MutableBloomFilter as M (new, size, isFull, insert)
import Prelude hiding (elem, notElem)
import Types

-- | Create a scalable bloom filter.
-- The first argument is the compounded false positive rate desired (between 0 and 1) (P)
-- We start off with an initial capacity of 1024 elements
new :: Hashable a => Double -> ST s (ScalableBloom s a)
new p = do
    mb <- M.new p0 1024 -- initial capacity of 1024 elements
    initArray <- newArray (0, 0) mb
    return $ ScalableBloom 1024 p initArray
    where p0 = p * (1-r) -- P converges to <= P0/(1-r). Thus, P0 = P*(1-r)
          r = 0.9

-- | Returns the total size (M = m*k) in bits of the filter.
-- This is equal to the individual sizes of the constituent
-- filters.
size :: ScalableBloom s a -> ST s Word32
size filt = do
    filters <- getElems (filtArray filt)
    fmap sum $ mapM M.size filters

-- | Inserts an element into the filter.
-- The first argument is the filter
-- The second argument is the element to insert
insert :: Hashable a => ScalableBloom s a -> a -> ST s ()
insert filt element = do
    filters <- getElems (filtArray filt)
    let lastMb = last filters
        p = (mutErrRate lastMb) * r -- Pi = Pi-1 * r
        c = (mutCap lastMb) * s     -- Mi = Mi-1 * s
        r = 0.9
        s = 2
    lastFilterIsFull <- M.isFull lastMb
    if lastFilterIsFull then do
        -- create a new bloom filter, add the element to it and add the filter itself
        -- to the array of bloom filters in the scalable filter
        newFilter <- M.new p c
        M.insert newFilter element
        writeArray (filtArray filt) (length filters) newFilter
    else M.insert lastMb element

-- | Inserts multiple elements into the filter.
-- The first argument is the filter
-- The second argument is the list of elements to insert
insertList :: Hashable a => ScalableBloom s a -> [a] -> ST s ()
insertList filt = mapM_ (insert filt)

-- | Returns True if the element is in the filter
elem :: ScalableBloom s a -> a -> ST s Bool
elem filt element = undefined

-- | Complement of 'elem'.
notElem :: ScalableBloom s a -> a -> ST s Bool
notElem filt element = undefined
