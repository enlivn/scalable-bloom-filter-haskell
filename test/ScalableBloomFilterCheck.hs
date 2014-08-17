{-|
Module      : ScalableBloomFilterCheck
Description : QuickCheck tests for Scalable bloom filter
-}

module ScalableBloomFilterCheck where

import Control.Monad.ST (runST)
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.STRef (readSTRef)
import Hash.Hash
import qualified ScalableBloomFilter as SB
import TestTypes
import Types

-- an element that was inserted is always found
prop_elem_inserted_always_exists :: Hashable a => FalsePositiveRate -> a -> Bool
prop_elem_inserted_always_exists (FalsePositiveRate falsePositiveRate)
                                 element = runST $ SB.fromList falsePositiveRate [element] >>=
                                                   flip SB.elem element

-- inserting an appropriate number of elements causes the filter to scale up
prop_filter_scales  :: NumElemsToForceScaling -> Bool
prop_filter_scales (NumElemsToForceScaling numElems) = runST $
    SB.fromList 0.01 testElems >>= \filt -> do
    readSTRef (filtArray filt) >>= return . (<) 1 . length
    where testElems :: [Int]
          testElems = [1..numElems] -- make sure we generate unique elements
                                    -- to minimize false positives

-- inserting a number of elements <= the capacity of the first constituent
-- filter (1024) means the scalable filter will not scale up
prop_filter_does_not_scale  :: NumElemsForNoScaling -> Bool
prop_filter_does_not_scale (NumElemsForNoScaling numElems) = runST $
    SB.fromList 0.01 testElems >>= \filt -> do
    readSTRef (filtArray filt) >>= return . (==) 1 . length
    where testElems :: [Int]
          testElems = [1..numElems] -- make sure we generate unique elements to minimize false positives

main :: IO ()
main = do
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Char -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Int -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Float -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Double -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> String -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> L.ByteString -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> S.ByteString -> Bool)
    runNTests 500 (prop_filter_scales :: NumElemsToForceScaling -> Bool)
    runNTests 500 (prop_filter_does_not_scale :: NumElemsForNoScaling -> Bool)
