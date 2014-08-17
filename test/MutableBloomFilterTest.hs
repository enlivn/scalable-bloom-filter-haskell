{-|
Module      : MutableBloomFilterTest
Description : QuickCheck tests for Mutable bloom filter
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MutableBloomFilterTest where

import Control.Monad (liftM)
import Control.Monad.ST (runST)
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Hash.Hash
import qualified MutableBloomFilter as M
import TestTypes

-- an element that was inserted is always found
prop_elem_inserted_always_exists :: Hashable a => FalsePositiveRate -> a -> Bool
prop_elem_inserted_always_exists (FalsePositiveRate falsePositiveRate)
                                 element = runST $ M.fromList falsePositiveRate [element] >>=
                                                   flip M.elem element

-- filter count must be either equal to or smaller than the number of elements inserted
-- (because the count will not be incremented for the elements ignored due to false
-- positives)
prop_filt_count_is_less_than_or_equal_num_elems_inserted :: LimitNumElems -> Bool
prop_filt_count_is_less_than_or_equal_num_elems_inserted (LimitNumElems numElems) =
        runST $ M.fromList 0.01 testElems >>=
        liftM ((>=) (fromIntegral numElems)) . M.getCount
        where testElems = [1..numElems] -- make sure we generate unique elements to minimize false
                                        -- positives

main :: IO ()
main = do
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Char -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Int -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Float -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Double -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> String -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> L.ByteString -> Bool)
    runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> S.ByteString -> Bool)
    runNTests 500 (prop_filt_count_is_less_than_or_equal_num_elems_inserted :: LimitNumElems -> Bool)

--main = do
--    let fp = 0.3080696608225276
--        n  = 2100937750 :: Word32
--        --res :: Bool
--        --res = runST $ (M.new fp n::ST s (M.MutableBloom s Int)) >>= (flip M.elem) 20
--        mbps = runST $ mutBitsPerSlice `fmap` (M.new fp n::ST s (M.MutableBloom s Int))
 --   --print res
 --   print mbps
--    res <- return $ (prop_elem_inserted_exists :: FalsePositiveRate -> String -> Bool) (FalsePositiveRate fp) "a"
--    print res
