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
                           size,
                           insert,
                           insertList,
                           elem,
                           notElem) where

import Control.Monad.ST (ST, runST)
import Data.Word (Word32)
import Hash.Hash
import qualified MutableBloomFilter as M (new, size)
import Prelude hiding (elem, notElem)
import Types

-- | Create a scalable bloom filter.
-- The first argument is the compounded false positive rate desired (between 0 and 1) (P)
-- We start off with an initial capacity of 1024 bits
new :: Hashable a => Double -> ST s (ScalableBloom s a)
new p = do
    mb <- M.new p0 1024
    return $ ScalableBloom 1024 p [mb]
    where p0 = p * (1-r)
          r = 0.9

-- | Returns the total size (M = m*k) in bits of the filter.
-- This is equal to the individual sizes of the constituent
-- filters.
size :: ScalableBloom s a -> ST s Word32
size filt = fmap sum $ mapM M.size (filtArray filt)

-- | Inserts an element into the filter.
-- The first argument is the filter
-- The second argument is the element to insert
insert :: ScalableBloom s a -> a -> ST s ()
insert filt element = undefined

-- | Inserts multiple elements into the filter.
-- The first argument is the filter
-- The second argument is the list of elements to insert
insertList :: ScalableBloom s a -> [a] -> ST s ()
insertList filt = undefined

-- | Returns True if the element is in the filter
elem :: ScalableBloom s a -> a -> ST s Bool
elem filt element = undefined

-- | Complement of 'elem'.
notElem :: ScalableBloom s a -> a -> ST s Bool
notElem filt element = undefined
